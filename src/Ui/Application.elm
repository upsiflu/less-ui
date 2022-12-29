module Ui.Application exposing (application, Application, Document, Msg)

{-|

@docs application, Application, Document, Msg

-}

import Browser
import Browser.Navigation as Nav
import Html
import Html.Attributes exposing (..)
import Ui exposing (Ui)
import Ui.Layout exposing (Layout)
import Ui.Link as Href exposing (Path)
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
-}
application :
    { init : ( model, Cmd modelMsg )
    , update : modelMsg -> model -> ( model, Cmd modelMsg )
    , view : Path -> model -> { body : Ui modelMsg, layout : Layout, title : String }
    }
    ---> Application model modelMsg
    -> Program () ( Nav.Key, Url, model ) (Msg modelMsg)
application config =
    Browser.application
        { init =
            \_ url key ->
                config.init
                    |> (\( updatedModel, modelCmd ) ->
                            ( ( key, canonical.init url, updatedModel ), Cmd.map ModelMsg modelCmd )
                       )
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = \_ -> Sub.none
        , update =
            \msg ( key, url, model ) ->
                case msg of
                    UrlChanged receivedUrl ->
                        let
                            canonicalUrl : Url
                            canonicalUrl =
                                canonical.update receivedUrl url
                        in
                        ( ( key, canonicalUrl, model )
                        , if canonicalUrl == url then
                            Cmd.none

                          else
                            Nav.replaceUrl key (Url.toString canonicalUrl)
                        )

                    LinkClicked (Browser.Internal newUrl) ->
                        ( ( key, url, model ), Nav.pushUrl key (Url.toString newUrl) )

                    LinkClicked (Browser.External href) ->
                        ( ( key, url, model ), Nav.load href )

                    ModelMsg modelMsg ->
                        config.update modelMsg model
                            |> (\( updatedModel, modelCmd ) ->
                                    ( ( key, url, updatedModel ), Cmd.map ModelMsg modelCmd )
                               )
        , view =
            \( _, url, model ) ->
                config.view url.path model
                    |> (\document ->
                            { title = document.title
                            , body = Ui.view url document.layout document.body |> List.map (Tuple.second >> Html.map ModelMsg)
                            }
                       )
        }


{-| Use cases 1:

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
canonical : { init : Url -> Url, update : Url -> Url -> Url }
canonical =
    { init = \received -> received
    , update = Href.update
    }



---- Update ----


{-| -}
type Msg modelMsg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | ModelMsg modelMsg
