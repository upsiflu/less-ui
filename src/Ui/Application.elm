module Ui.Application exposing (application, Application, Document, Msg)

{-| In contrast to `Browser.Application`, this module maks the `Url` the
single source of truth for the state of your user interface.

@docs application, Application, Document, Msg

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
import Html
import Ui exposing (Ui)
import Ui.Layout exposing (Layout)
import Ui.Link exposing (Fragment, Path)
import Ui.State
import Url exposing (Url)


{-| An Api for `href` composition!

Discussion: Is `Handle` just for the links in the top area of the
app, or for all Flag manipulating links (including tabs inside
the Control), or for all internal links?

  - It is very reassuring to know which layout area a node is drawn in

  - Nesting a Scene in a Control means that the Control 'controls' the
    Scene. So it may be feasible to say:

    Scene a
    Handle h (with some static content)
    Control c
    Scene b

which means, Scene a contains Scene b if Handle h is `on`
(Control c is transparent)

  - We don't know enough about links. It would be smart to visualise
    all hrefs in the google docs example.
    For example, the Menu link implements a dropdown, i.e. 'spring-
    loaded' state, so the flag must auto-delete when a new context is
    opened.

-}
type alias Application model modelMsg =
    Program () ( Nav.Key, Url, model ) (Msg modelMsg)


{-| -}
type alias Document modelMsg =
    { body : Ui modelMsg, layout : Layout, title : String }


{-| Separate Url update from Model update


### Lifecycle

    Opened Url
    in new tab           --> init   (initial)

    Clicked
    internal link        --> update Relative Link

    Manually replaced
    url                  --> update Absolute Link

-}
application :
    { init : ( model, Cmd modelMsg )
    , update : modelMsg -> model -> ( model, Cmd modelMsg )
    , view : ( Path, Fragment ) -> model -> { body : Ui modelMsg, layout : Layout, title : String }
    }
    -> Application model modelMsg
application config =
    Browser.application
        { init =
            \_ state key ->
                config.init
                    |> (\( updatedModel, modelCmd ) ->
                            ( ( key, Ui.State.init state, updatedModel )
                            , Cmd.batch [ Cmd.map ModelMsg modelCmd, Nav.replaceUrl key (Url.toString state) ]
                            )
                       )
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = \_ -> Sub.none
        , update =
            \msg ( key, state, model ) ->
                case msg of
                    UrlChanged receivedUrl ->
                        if receivedUrl == state then
                            ( ( key, state, model ), Cmd.none )

                        else
                            Ui.State.update (Ui.Link.fromUrl receivedUrl) state
                                |> (\canonicalState ->
                                        ( ( key, canonicalState, model )
                                        , if canonicalState == state then
                                            Cmd.none

                                          else
                                            Nav.replaceUrl key (Ui.State.toUrlString canonicalState)
                                        )
                                   )

                    LinkClicked (Browser.Internal url) ->
                        ( ( key, state, model )
                        , url
                            |> Ui.Link.fromUrl
                            |> Ui.Link.relative
                            |> Ui.Link.toUrlString
                            |> Nav.pushUrl key
                        )

                    LinkClicked (Browser.External href) ->
                        ( ( key, state, model ), Nav.load href )

                    ModelMsg modelMsg ->
                        config.update modelMsg model
                            |> (\( updatedModel, modelCmd ) ->
                                    ( ( key, state, updatedModel ), Cmd.map ModelMsg modelCmd )
                               )
        , view =
            \( _, url, model ) ->
                config.view ( url.path, url.fragment ) model
                    |> (\document ->
                            { title = document.title
                            , body = Ui.view url document.layout document.body |> List.map (Tuple.second >> Html.map ModelMsg)
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
   Now, `canonical.init` canonicalises the initial Url

-}
---- Update ----


{-| -}
type Msg modelMsg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | ModelMsg modelMsg
