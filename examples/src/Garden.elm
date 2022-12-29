module Garden exposing
    ( main
    , Msg
    )

{-|

@docs main

-}

import Garden.Rhododendron exposing (Rhododendron)
import Html exposing (Html)
import Html.Attributes as Attr
import Ui exposing (Ui)
import Ui.Application exposing (Application, application)
import Ui.Layout as Layout exposing (Layout)
import Ui.Layout.Aspect exposing (Aspect(..))
import Ui.Link as Link


{-| -}
type Msg
    = RhododendronMsg Garden.Rhododendron.Msg


type alias Garden =
    { rhododendron : Rhododendron }


init : ( Garden, Cmd Msg )
init =
    ( { rhododendron = Garden.Rhododendron.singleton "Root" }, Cmd.none )


update : Msg -> Garden -> ( Garden, Cmd Msg )
update msg garden =
    case msg of
        RhododendronMsg rhodoMsg ->
            ( { garden | rhododendron = Garden.Rhododendron.update rhodoMsg garden.rhododendron }
            , Cmd.none
            )


view : Link.Path -> Garden -> { body : Ui Msg, layout : Layout, title : String }
view path garden =
    { title = "Welcome to the Garden!"
    , layout = Layout.Default
    , body =
        page
            |> Ui.with Scene
                (Ui.link [ Control ] (Link.Toggle "rhododendron")
                    |> Ui.with Control
                        (Garden.Rhododendron.view RhododendronMsg garden.rhododendron)
                )
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
main : Application Garden Msg
main =
    application
        { init = init
        , update = update
        , view = view
        }
