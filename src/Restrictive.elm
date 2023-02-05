module Restrictive exposing
    ( application, Application, Document, Msg
    , toggle, goTo, bounce
    )

{-| In contrast to `Browser.Application`, this module maks the `Url` the
single source of truth for the state of your user interface.

@docs application, Application, Document, Msg


# Links

@docs toggle, goTo, bounce

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
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout exposing (Layout)
import Restrictive.Layout.Region as Region exposing (OrHeader(..))
import Restrictive.Mask as Mask exposing (Mask)
import Restrictive.State as State exposing (Flag, Fragment, Path, State)
import Restrictive.Ui as Ui exposing (Ui)
import Url exposing (Url)


{-| -}
type alias Application model modelMsg =
    Program () ( Nav.Key, State, model ) (Msg modelMsg)


{-| -}
type alias Document aspect html =
    { body : Ui aspect html, layout : Layout aspect html, title : String }


{-| Separate Url update from Model update


### Lifecycle

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
    , view : model -> Document aspect ( String, Html modelMsg )
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
                , Cmd.batch [ Cmd.map ModelMsg modelCmd, Nav.replaceUrl key (Url.toString initialState.current) ]
                )
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = \_ -> Sub.none
        , update =
            \msg ( key, state, model ) ->
                let
                    updateUrl : State.Link -> ( ( Nav.Key, { current : Url, previous : Maybe Url }, model ), Cmd msg )
                    updateUrl link =
                        State.toStateTransition link state.current
                            |> (\canonicalState ->
                                    ( ( key, { state | current = canonicalState, previous = Just state.current }, model )
                                    , if state.current == canonicalState then
                                        Cmd.none

                                      else if canonicalState.path == state.current.path && canonicalState.fragment == state.current.fragment then
                                        Nav.replaceUrl key (State.toUrlString canonicalState)

                                      else
                                        Nav.pushUrl key (State.toUrlString canonicalState)
                                    )
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
                config.view model
                    |> (\document ->
                            { title = document.title
                            , body =
                                Ui.view state document.layout document.body
                                    |> List.map (Tuple.second >> Html.map ModelMsg)
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


{-| -}
bounce : { there : ( Maybe Path, Fragment ), here : ( Maybe Path, Fragment ) } -> Ui aspect ( String, Html msg )
bounce =
    State.bounce
        >> State.inlineLink
        >> (<<)
            (\{ linkHtml, occlude } ->
                Mask.mapKey ( Region.justRegion, Region.Region >> Just ) occlude
                    >> Get.append (Get.map Ui.foliage linkHtml)
            )
        >> Ui.custom


{-| -}
goTo : ( Maybe Path, Fragment ) -> Ui aspect ( String, Html msg )
goTo =
    State.goTo
        >> State.inlineLink
        >> (<<)
            (\{ linkHtml, occlude } ->
                Mask.mapKey ( Region.justRegion, Region.Region >> Just ) occlude
                    >> Get.append (Get.map Ui.foliage linkHtml)
            )
        >> Ui.custom


{-| -}
toggle : Flag -> Ui aspect ( String, Html msg )
toggle =
    State.toggle
        >> State.inlineLink
        >> (<<)
            (\{ linkHtml, occlude } ->
                Mask.mapKey ( Region.justRegion, Region.Region >> Just ) occlude
                    >> Get.append (Get.map Ui.foliage linkHtml)
            )
        >> Ui.custom
