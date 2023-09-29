module Garden exposing (Garden, main, Msg)

{-|

@docs Garden, main, Msg

-}

import Garden.Rhododendron as Rhododendron exposing (Rhododendron)
import Html exposing (Html)
import Html.Attributes as Attr
import Less
import Less.Ui as Ui
import Less.Ui.Html
import Less.Ui.Region exposing (Region(..))


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
    Less.Ui.Html.Ui Msg Msg


view : Garden -> Less.Document Msg
view garden =
    Less.mapDocument identity
        { body =
            page
                ++ Ui.at Scene
                    (Less.Ui.Html.toggle []
                        { flag = "rhododendron"
                        , inHeader = True
                        , label = [ Html.text "Toggle Rhododendron" ]
                        }
                        (Rhododendron.view RhododendronMsg garden.rhododendron)
                    )
        , layout = Less.Ui.Html.layout
        , title = "Welcome to the Garden!"
        }


page : Ui
page =
    Ui.singleton [ Html.text "Handle" ]
        ++ myScene "Scene 1"
        ++ myScene "Scene 2"
        ++ Ui.at Control myControl
        ++ Ui.at Info myInfo


square : String -> Html msg
square color =
    Html.div
        [ Attr.style "background" color
        , Attr.style "height" "200px"
        , Attr.style "width" "200px"
        ]
        []


textLabel : String -> Ui
textLabel str =
    Ui.singleton [ Html.text str ]


myScene : String -> Ui
myScene sId =
    Ui.singleton [ square "red" ]
        ++ Ui.singleton [ square "blue" ]
        |> Less.Ui.Html.section [ Attr.id sId ]
        |> Ui.at Scene


myControl : Ui
myControl =
    textLabel "Control"


myInfo : Ui
myInfo =
    textLabel "Info"


{-| -}
main : Less.Application Garden Msg
main =
    Less.application
        { init = init
        , update = update
        , view = view
        }
