module Restrictive exposing (application, Application, Document, Msg, mapDocument)

{-| In contrast to `Browser.Application`, this module maks the `Url` the
single source of truth for the state of your user interface.

@docs application, Application, Document, Msg, mapDocument

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

    - - - - - - Url ↘     ↗ ↗ Url' - - -
                     Flags, Path
                               ↘
                               view
                               ↗
    - - - - -  Model ↘    ↗ Model' - - -
                     update
             Message ↗    ↘ Cmd

-}

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Restrictive.State as State exposing (State)
import Restrictive.Ui as Ui exposing (Layout, Ui)
import Url exposing (Url)


{-| -}
type alias Application model modelMsg =
    Program () ( Nav.Key, State, model ) (Msg modelMsg)


{-| -}
type alias Document msg =
    State -> Browser.Document msg


{-| -}
mapDocument :
    (html -> List (Html msg))
    -> { body : Ui region html wrapper, layout : Layout region narrowHtml_ html narrowWrapper_ wrapper, title : String }
    -> (State -> Browser.Document msg)
mapDocument toHtml document =
    \state ->
        { title = document.title
        , body =
            Ui.view state document.layout document.body
                |> toHtml
        }


{-| An `Html` application for the Elm Browser runtime


## Separate Url update from Model update


### Lifecycle:

    Opened Url
    in new tab           -> init   (initial)

    Clicked
    internal link        -> update Relative Link

    Manually replaced
    url                  -> update Absolute Link

-}
application :
    { init : ( model, Cmd modelMsg )
    , update : modelMsg -> model -> ( model, Cmd modelMsg )
    , view : model -> (State -> Browser.Document modelMsg)
    }
    -> Application model modelMsg
application config =
    Browser.application
        { init =
            \_ url key ->
                let
                    ( ( updatedModel, modelCmd ), initialState ) =
                        ( config.init, State.init url )
                in
                ( ( key, initialState, updatedModel )
                , Cmd.batch
                    [ Cmd.map ModelMsg modelCmd
                    , Nav.replaceUrl key (Url.toString initialState.current)
                    ]
                )
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = \_ -> Sub.none
        , update =
            \msg ( key, state, model ) ->
                let
                    updateUrl : State.Link -> ( ( Nav.Key, State, model ), Cmd msg )
                    updateUrl link =
                        let
                            next : Url
                            next =
                                State.toStateTransition link state.current
                        in
                        ( ( key, { state | current = next, previous = Just state.current }, model )
                        , if state.current == next then
                            Cmd.none

                          else if next.path == state.current.path && next.fragment == state.current.fragment then
                            Nav.replaceUrl key (State.toUrlString next)

                          else
                            Nav.pushUrl key (State.toUrlString next)
                        )
                in
                case msg of
                    UrlChanged url ->
                        if url == state.current then
                            ( ( key, state, model ), Cmd.none )

                        else
                            State.getLink state.current.path url
                                |> updateUrl

                    LinkClicked (Browser.Internal url) ->
                        if url == state.current then
                            ( ( key, state, model ), Cmd.none )

                        else
                            State.getLink state.current.path url
                                |> State.relative
                                |> updateUrl

                    LinkClicked (Browser.External href) ->
                        ( ( key, state, model ), Nav.load href )

                    ModelMsg modelMsg ->
                        config.update modelMsg model
                            |> (\( updatedModel, modelCmd ) ->
                                    ( ( key, state, updatedModel ), Cmd.map ModelMsg modelCmd )
                               )
        , view =
            \( _, state, model ) ->
                config.view model state
                    |> (\document ->
                            { title = document.title
                            , body = List.map (Html.map ModelMsg) document.body
                            }
                       )
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
---- Update ----


{-| -}
type Msg modelMsg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | ModelMsg modelMsg
