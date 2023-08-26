module Garden exposing (Garden, main, Msg)

{-|

@docs Garden, main, Msg

-}

import Garden.Rhododendron as Rhododendron exposing (Rhododendron)
import Html exposing (Html)
import Html.Attributes as Attr
import Restrictive
import Restrictive.Layout.Html.Keyed as Keyed exposing (Wrapper(..))
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
    Keyed.Ui Msg


view : Garden -> Restrictive.Document Msg
view garden =
    Restrictive.mapDocument Keyed.toHtml
        { body =
            page
                ++ Ui.at Scene
                    (Ui.toggle []
                        { flag = "rhododendron"
                        , isInline = True
                        , label = [ ( "Label", Html.text "Toggle Rhododendron" ) ]
                        }
                        (Rhododendron.view RhododendronMsg garden.rhododendron)
                    )
        , layout = Keyed.layout
        , title = "Welcome to the Garden!"
        }


page : Ui
page =
    Ui.singleton [ ( "constant", Html.text "Handle" ) ]
        ++ myScene "Scene 1"
        ++ myScene "Scene 2"
        ++ Ui.at Control myControl
        ++ Ui.at Info myInfo


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


textLabel : String -> Ui
textLabel str =
    Ui.singleton [ ( str, Html.text str ) ]


myScene : String -> Ui
myScene sId =
    Ui.singleton [ square "red" ]
        ++ Ui.singleton [ square "blue" ]
        |> Ui.wrap (Node "section" [ Attr.id sId ])
        |> Ui.at Scene


myControl : Ui
myControl =
    textLabel "Control"


myInfo : Ui
myInfo =
    textLabel "Info"



-- Field `view` expected `Document -> State -> Document`
--, found                `Document Region (Keyed msg) (Attribute Never) (Wrapper Msg)`Elm


{-| -}
main : Restrictive.Application Garden Msg
main =
    Restrictive.application
        { init = init
        , update = update
        , view = view
        }
