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
import Restrictive.Link as Link exposing (Link, Msg(..), State)
import Restrictive.Ui as Ui exposing (Layout, Ui)
import Return exposing (Return)
import Url


type alias ApplicationState model =
    ( Nav.Key, { current : State, previous : Maybe State }, model )


{-| -}
type alias Application model modelMsg =
    Program () (ApplicationState model) (Msg modelMsg)


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
    \states ->
        { title = document.title
        , body =
            Ui.view (Ui.applyStates states document.layout) document.body
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
    , update : modelMsg -> model -> Return modelMsg model
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
            \msg ->
                let
                    applyLink : Link -> ApplicationState model -> Return msg_ (ApplicationState model)
                    applyLink link ( key, { current } as state, model ) =
                        let
                            ( { pushHistoryState }, newState ) =
                                Link.apply link current
                        in
                        nextState newState ( key, state, model )
                            |> Return.command
                                (if pushHistoryState then
                                    Nav.pushUrl key (Url.toString newState)

                                 else
                                    Nav.replaceUrl key (Url.toString newState)
                                )

                    nextState : State -> ApplicationState model -> Return msg_ (ApplicationState model)
                    nextState newUrl ( key, { current }, model ) =
                        Return.singleton ( key, { current = newUrl, previous = Just current }, model )

                    updateModel : modelMsg -> ApplicationState model -> Return (Msg modelMsg) (ApplicationState model)
                    updateModel modelMsg ( key, state, model ) =
                        let
                            ( updatedModel, modelCmd ) =
                                config.update modelMsg model
                        in
                        Return.return ( key, state, updatedModel ) (Cmd.map AppMsg modelCmd)
                in
                case msg of
                    UrlChanged url ->
                        nextState url

                    LinkClicked (Browser.Internal url) ->
                        Link.fromUrl url
                            |> Maybe.map applyLink
                            |> Maybe.withDefault Return.singleton

                    LinkClicked (Browser.External href) ->
                        Return.singleton
                            >> Return.command (Nav.load href)

                    AppMsg modelMsg ->
                        updateModel modelMsg

                    UrlCmd link ->
                        applyLink link
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
