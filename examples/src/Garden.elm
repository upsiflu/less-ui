module Garden exposing (Garden, main, Msg)

{-|

@docs Garden, main, Msg

-}

import Garden.Rhododendron as Rhododendron exposing (Rhododendron)
import Html exposing (Html)
import Html.Attributes as Attr
import Ui exposing (Ui)
import Ui.Application exposing (Application, application)
import Ui.Layout as Layout exposing (Layout)
import Ui.Layout.Aspect exposing (Aspect(..))
import Ui.Link
import Ui.State


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


view : ( Ui.State.Path, Ui.State.Fragment ) -> Garden -> { body : Ui (Html Msg), layout : Layout (Html Msg), title : String }
view _ garden =
    { body =
        page
            |> Ui.with Scene
                (Ui.Link.toggle "rhododendron"
                    |> Ui.Link.view (Ui.Link.preset.global [] [ Html.text "rhododendron" ])
                    |> Ui.with Control
                        (Rhododendron.view RhododendronMsg garden.rhododendron)
                )
    , layout = Layout.default
    , title = "Welcome to the Garden!"
    }


page : Ui (Html Msg)
page =
    Ui.handle [ Html.text "Handle" ]
        |> Ui.with Scene (myScene "Scene 1")
        |> Ui.with Scene (myScene "Scene 2")
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


myScene : String -> Ui (Html msg)
myScene sId =
    Ui.html (square "red")
        ++ Ui.html (square "blue")
        |> Ui.addTextLabel sId
        |> Ui.node "section" sId


myControl : Ui (Html msg)
myControl =
    Ui.textLabel "Control"


myInfo : Ui (Html msg)
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
