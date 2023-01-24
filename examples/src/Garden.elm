module Garden exposing (Garden, main, Msg)

{-|

@docs Garden, main, Msg

-}

import Garden.Rhododendron as Rhododendron exposing (Rhododendron)
import Html exposing (Html)
import Html.Attributes as Attr
import Ui
import Ui.Application exposing (Application, application)
import Ui.Layout as Layout
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


type alias Ui =
    Ui.Ui Aspect ( String, Html Msg ) (List ( String, Html Msg ) -> List ( String, Html Msg ))


type alias Document =
    Ui.Application.Document Aspect ( String, Html Msg ) (List ( String, Html Msg ) -> List ( String, Html Msg ))


view : ( Ui.State.Path, Ui.State.Fragment ) -> Garden -> Document
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
