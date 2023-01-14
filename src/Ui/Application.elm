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
import Ui.Link as Link exposing (Fragment, Path)
import Ui.State as State exposing (State)
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
    Program () ( Nav.Key, State, model ) (Msg modelMsg)


{-| -}
type alias Document modelMsg =
    { body : Ui modelMsg, layout : Layout, title : String }


{-| Separate Url update from Model update

**Routing:**
Use `Link` to generate clickable internal hrefs.
If you share a `Link`, it highlights its source.

TODO: I don't know what is useful...

The state is really the relevant thing.
It is encoded in the Url and contains all the stuff.

    link
    href                 state:url                         new state

    toggle "x"           path#fragment?flags&assign=ments  path#fragment?flags&x&assign=ments

    toggle absolute "x"
    ?!&toggle="x"        p#f?x&a=b -> p#f?x&a=b

-}
application :
    { init : ( model, Cmd modelMsg )
    , update : modelMsg -> model -> ( model, Cmd modelMsg )
    , view : ( Path, Fragment ) -> model -> { body : Ui modelMsg, layout : Layout, title : String }
    }
    ---> Application model modelMsg
    -> Program () ( Nav.Key, State, model ) (Msg modelMsg)
application config =
    Browser.application
        { init =
            \_ state key ->
                config.init
                    |> (\( updatedModel, modelCmd ) ->
                            ( ( key, state |> Debug.log "Application.init state:" |> State.init |> Debug.log "|> State.init:", updatedModel )
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
                        if
                            (receivedUrl
                                |> Debug.log "Application.update UrlChanged"
                            )
                                == state
                        then
                            ( ( key, state, model ), Cmd.none )

                        else
                            State.update (Link.fromUrl receivedUrl |> Debug.log "Application.update Url Changed ---> Link.fromUrl ") state
                                |> Debug.log "     updated:"
                                |> (\canonicalState ->
                                        ( ( key, canonicalState, model )
                                        , if canonicalState == state then
                                            Cmd.none

                                          else
                                            Nav.replaceUrl key (State.toUrlString canonicalState)
                                        )
                                   )

                    LinkClicked (Browser.Internal url) ->
                        -- Any clicked link needs to be interpreted as relative
                        ( ( key, state, model )
                        , url
                            |> Debug.log "Href clicked:"
                            |> Link.fromUrl
                            |> Debug.log "Equals link:"
                            |> Link.relative
                            |> Debug.log "Make relative:"
                            |> Link.toUrlString
                            |> Debug.log "To Url String (and pushUrl):"
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
