module Restrictive exposing (application, Application, mapDocument, Document)

{-| Makes the `Url` the single source of truth for the state of your user interface,
and hides the corresponding messages from your `update`.

@docs application, Application, mapDocument, Document

---


## Why?

In the conventional Elm architecture, if you want the `view` to respect state encoded in the `Url` such as queries or flags, you need your `update` to incorporate this state into the in-memory application Model, which is eventually interpreted in the `view`:

     - - Url ↘               Cmd ⭢ Url' - - -
             Message ↘      ↗
                      update
     - - - - - Model ↗      ↘ Model' - - - - -
                                  ↘
                                  view

This opens two possible pitfalls:

  - The Url can potentially manipulate parts of the Model that are not only intended for viewing, for example data shared with the backend
  - The Model needs to parse the `Url` and pass it to the `view`, potentially losing or misinterpreting information.


## How?

    - - - - - - Url ↘              ↗ Url' - -
                 Path, Flags, Fragment
                               ↘
                              view
             Message ↘         ↗
    - - - - -  Model ↘    ↗ Model' - - - - -
                     update
                          ↘ Cmd

-}

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Restrictive.Msg exposing (Msg(..))
import Restrictive.State as State exposing (State)
import Restrictive.Ui as Ui exposing (Layout, Ui)
import Url


{-| -}
type alias Application model modelMsg =
    Program () ( Nav.Key, { current : State, previous : Maybe State }, model ) (Msg modelMsg)


{-| Parametrizes [Browser.Document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#Document)
over the current [`State`](Restrictive.State)
-}
type alias Document msg =
    { current : State, previous : Maybe State } -> Browser.Document msg


{-| -}
mapDocument :
    (html -> List (Html msg))
    -> { body : Ui region html wrapper, layout : Layout region narrowHtml_ html narrowWrapper_ wrapper, title : String }
    -> Document msg
mapDocument toHtml document =
    \state ->
        { title = document.title
        , body =
            Ui.view state document.layout document.body
                |> toHtml
        }


{-| Create a standalone `Html` application for the Elm Browser runtime.


## App Lifecycle:

    Opened Url
    in new tab           -> init

    Clicked
    internal link        -> update Relative Link

    Manually replaced
    url                  -> update Absolute Link

-}
application :
    { init : ( model, Cmd modelMsg )
    , update : modelMsg -> model -> ( model, Cmd modelMsg )
    , view : model -> ({ current : State, previous : Maybe State } -> Browser.Document modelMsg)
    }
    -> Application model modelMsg
application config =
    Browser.application
        { init =
            \_ url key ->
                let
                    ( ( initialModel, modelCmd ), initialState ) =
                        ( config.init, url )
                in
                ( ( key, { current = initialState, previous = Nothing }, initialModel )
                , Cmd.batch
                    [ Cmd.map AppMsg modelCmd
                    , Nav.replaceUrl key (Url.toString initialState)
                    ]
                )
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = \_ -> Sub.none
        , update =
            \msg ( key, { current } as state, model ) ->
                let
                    updateUrl : State.Link -> ( ( Nav.Key, { current : State, previous : Maybe State }, model ), Cmd msg )
                    updateUrl link =
                        let
                            next : State
                            next =
                                State.next link current
                        in
                        ( ( key, { current = next, previous = Just current }, model )
                        , if current == next then
                            Cmd.none

                          else if next.path == current.path && next.fragment == current.fragment then
                            Nav.replaceUrl key (State.toString next)

                          else
                            Nav.pushUrl key (State.toString next)
                        )
                in
                case msg of
                    UrlChanged url ->
                        if url == current then
                            ( ( key, state, model ), Cmd.none )

                        else
                            State.getLink url current
                                |> updateUrl

                    LinkClicked (Browser.Internal url) ->
                        if url == current then
                            ( ( key, state, model ), Cmd.none )

                        else
                            State.getLink url current
                                |> State.relative
                                |> updateUrl

                    LinkClicked (Browser.External href) ->
                        ( ( key, state, model ), Nav.load href )

                    AppMsg modelMsg ->
                        let
                            ( updatedModel, modelCmd ) =
                                config.update modelMsg model
                        in
                        ( ( key, state, updatedModel ), Cmd.map AppMsg modelCmd )

                    UrlCmds assignments ->
                        ( ( key, state, model )
                        , List.foldl
                            State.integrateAssignment
                            current
                            assignments
                            |> State.toString
                            |> Nav.replaceUrl key
                        )

        {-
           Field `view` expected

           `( Key, { current : Url, previous : Maybe State }, model )
               -> Document (Msg modelMsg)`

           , found

           `( Key, State, model )
               -> { title : String, body : List (Html (Msg modelMsg)) }`
        -}
        , view =
            \( _, state, model ) ->
                let
                    { title, body } =
                        config.view model state
                in
                { title = title
                , body = List.map (Html.map AppMsg) body
                }
        }



{- Use cases 1:

   The user toggles a Ui handle, for example the avatar, to open or close a menu.

   (a)
   They decide to share the link of the handle, so they right-click on the toggle and choose 'copy link'.
   Their friend opens the link and the handle is activated.

   (b)
   They copy the Url and paste it in another tab or browser or device.
   The app loads and restores exactly the same Ui state.

   In the case of (a), we share a href, which is a string.
   The friend opens the link, and Elm turns it into the initial Url.
   Now, `canonical.init` canonicalises the initial Url.

-}
