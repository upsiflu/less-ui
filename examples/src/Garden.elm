module Garden exposing (Garden, main, Msg)

{-|

@docs Garden, main, Msg

-}

import Garden.Rhododendron as Rhododendron exposing (Rhododendron)
import Html exposing (Html)
import Html.Attributes as Attr
import Restrictive exposing (Application, application)
import Restrictive.Layout as Layout
import Restrictive.Layout.Region exposing (Region(..))
import Restrictive.Ui as Ui


{-| -}
type Msg
    = RhododendronMsg Rhododendron.Msg


{-| -}
type alias Garden =
    { rhododendron : Rhododendron }


init : ( Garden, Cmd Msg )
init =
    ( { rhododendron = Rhododendron.singleton "Root" }, Cmd.none )


update : Msg -> Garden -> ( Garden, Cmd Msg )
update msg garden =
    case msg of
        RhododendronMsg rhodoMsg ->
            ( { garden | rhododendron = Rhododendron.update rhodoMsg garden.rhododendron }
            , Cmd.none
            )


type alias Ui =
    Ui.Ui Region ( String, Html Msg )


type alias Document =
    Restrictive.Document Region ( String, Html Msg )


view : Garden -> Document
view garden =
    { body =
        page
            |> Ui.with Scene
                (Restrictive.toggle "rhododendron"
                    |> Ui.with Control
                        (Rhododendron.view RhododendronMsg garden.rhododendron)
                )
    , layout = Layout.default
    , title = "Welcome to the Garden!"
    }


page : Ui
page =
    Ui.handle [ ( "constant", Html.text "Handle" ) ]
        |> Ui.with Scene (myScene "Scene 1")
        |> Ui.with Scene (myScene "Scene 2")
        |> Ui.with Control myControl
        |> Ui.with Info myInfo


square : String -> ( String, Html msg )
square color =
    ( color
    , Html.div
        [ Attr.style "background" color
        , Attr.style "height" "200px"
        , Attr.style "width" "200px"
        ]
        []
    )


myScene : String -> Ui
myScene sId =
    Ui.html (square "red")
        ++ Ui.html (square "blue")
        |> Ui.addTextLabel sId
        |> Ui.node "section" sId


myControl : Ui
myControl =
    Ui.textLabel "Control"


myInfo : Ui
myInfo =
    Ui.textLabel "Info"



-- Field `view` expected `(Path, Fragment) -> Garden -> { body : Ui Msg, layout : Layout, title : String }`
--, found                `(Path, Fragment) -> Garden -> { body : Ui (Html Msg), layout : Layout, title : String }`Elm


{-| -}
main : Application Garden Msg
main =
    application
        { init = init
        , update = update
        , view = view
        }
