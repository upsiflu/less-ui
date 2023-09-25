module Less exposing (application, Application, mapDocument, Document)

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
import Less.Link as Link exposing (Link, Msg(..), State)
import Less.Ui as Ui exposing (Layout, Ui)
import Return exposing (Return)
import Url


{-| 🐌
-}
type alias Application model modelMsg =
    Program () (ApplicationState model) (Msg modelMsg)


type alias ApplicationState model =
    ( Nav.Key, { current : State, previous : Maybe State }, model )


{-| Parametrizes [Browser.Document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#Document)
over the current and previous [`State`](Less-Link#State).
-}
type alias Document msg =
    { current : State, previous : Maybe State } -> Browser.Document (Msg msg)


{-| Render the document and map it to `Html`.
-}
mapDocument :
    (html -> List (Html (Msg msg)))
    -> { body : Ui region html wrapper, layout : Layout region narrowHtml_ html narrowWrapper_ wrapper, title : String }
    -> Document msg
mapDocument toHtml { body, layout, title } =
    \states ->
        { title = title
        , body =
            Ui.view (Ui.applyStates states layout) body
                |> toHtml
        }


{-| Keeps the Ui state in the Url.
-}
application :
    { init : ( model, Cmd modelMsg )
    , update : modelMsg -> model -> Return modelMsg model
    , view : model -> ({ current : State, previous : Maybe State } -> Browser.Document (Msg modelMsg))
    }
    -> Application model modelMsg
application config =
    Browser.application
        { init =
            \_ url key ->
                let
                    ( initialModel, modelCmd ) =
                        config.init
                in
                ( ( key, { current = url, previous = Nothing }, initialModel )
                , Cmd.map AppMsg modelCmd
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
                        -- changing the Url will implicitly trigger `nextState`
                        Return.singleton ( key, state, model )
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
                            |> applyLink

                    LinkClicked (Browser.External href) ->
                        Return.singleton
                            >> Return.command (Nav.load href)

                    AppMsg modelMsg ->
                        updateModel modelMsg

                    UrlCmd link ->
                        applyLink link
        , view =
            \( _, states, model ) -> config.view model states
        }
