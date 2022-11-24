module Garden exposing (main)

{-|

@docs main

-}

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Ui exposing (Ui)
import Ui.Layout exposing (Layout)
import Ui.Layout.Aspect exposing (Aspect(..))


type Msg
    = Noop


type alias Model =
    ()


init : ( Model, Cmd Msg )
init =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


view : Ui.Path -> Model -> { body : Ui Msg, layout : Maybe Layout, title : String }
view _ _ =
    { title = "Welcome to the Garden!"
    , layout = Nothing
    , body = page
    }


page : Ui Msg
page =
    Ui.constant [ Html.text "Handle" ]
        |> Ui.with Scene myScene
        |> Ui.with Scene myScene
        |> Ui.with Control myControl
        |> Ui.with Info myInfo


square : String -> Html msg
square color =
    Html.div
        [ Attr.style "background" color
        , Attr.style "height" "200px"
        , Attr.style "width" "200px"
        ]
        []


myScene : Ui msg
myScene =
    Ui.html (square "red")
        ++ Ui.html (square "blue")


myControl : Ui msg
myControl =
    Ui.textLabel "Control"


myInfo : Ui msg
myInfo =
    Ui.textLabel "Info"


{-| -}
main : Ui.Application () Msg
main =
    Ui.application
        { init = init
        , update = update
        , view = view
        }
