module Features exposing (Features, main, Msg)

{-|

@docs Features, main, Msg

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Less
import Less.Ui as Ui
import Less.Ui.Html
import Less.Ui.Region exposing (Region(..))


{-| -}
type alias Msg =
    ()


{-| -}
type alias Features =
    {}


init : ( Features, Cmd Msg )
init =
    ( {}, Cmd.none )


update : Msg -> Features -> ( Features, Cmd Msg )
update () features =
    ( features, Cmd.none )



-- what happens is that when the state transitions to toggle off a feature,
-- then only the direct `Control` properly deprecates (poof) its elements
-- whereas all the others continue to be drawn.
--
-- The reason is that we implement hiding as `wrap`, which only
-- affects the `current Region`!


type alias Ui =
    Less.Ui.Html.Ui Msg Msg


textLabel : String -> Ui
textLabel str =
    Ui.singleton [ Html.text str ]


view : Features -> Less.Document Msg
view _ =
    let
        showTab : String -> Ui -> Ui
        showTab str contents =
            Less.Ui.Html.toggle []
                { flag = String.replace " " "+" str
                , isInline = False
                , label = [ Html.text str ]
                }
                contents
    in
    Less.mapDocument identity
        { body =
            textLabel "Toggle the features on top of the page! "
                ++ showTab "Flat-Ui-Layout"
                    ui
                ++ showTab "Global Navbar"
                    globalNav
        , layout = Less.Ui.Html.layout
        , title = "Less-Ui feature test"
        }


{-| [Ui](Ui): Flat layout instead of nested components
-}
ui : Ui
ui =
    textLabel "--Handle--"
        ++ Ui.at Scene (textLabel "--Scene--")
        ++ Ui.at Control (textLabel "--Control--")
        ++ Ui.at Info (textLabel "--Info--")


{-| [Application](Ui.Application): Sever Route from Model
-}
paths : Ui
paths =
    Less.Ui.Html.goTo []
        { destination = "Path-1"
        , isInline = True
        , label = [ Html.text "Path-1" ]
        }
        []
        ++ Less.Ui.Html.goTo []
            { destination = "Path-2"
            , isInline = True
            , label = [ Html.text "Path-2" ]
            }
            []
        ++ Less.Ui.Html.goTo []
            { destination = ""
            , isInline = True
            , label = [ Html.text "Nothing" ]
            }
            []


globalNav : Ui
globalNav =
    [ "Introduction", "First Steps", "Last Steps" ]
        |> List.concatMap
            (\title ->
                Less.Ui.Html.goTo []
                    { destination = title
                    , isInline = False
                    , label = [ Html.text title ]
                    }
                    (textLabel title)
            )


{-| [Link](Ui.Link): Manage the Ui State as a URL
-}
fragments : String -> Ui
fragments fr =
    let
        articles : List (Html msg)
        articles =
            [ Html.article [ Attr.id "1", Attr.tabindex 1 ]
                [ Html.p [] [ Html.text "Officiis tractatos at sed. Vim ad ipsum ceteros. Posse adolescens ei eos, meliore albucius facilisi id vel, et vel tractatos partiendo. Cu has insolens constituam, sint ubique sit te, vim an legimus elaboraret. Omnes possim mei et. Equidem contentiones vituperatoribus ut vel, duis veri platonem vel ei, an integre consequat democritum qui." ] ]
            , Html.article [ Attr.id "2", Attr.tabindex 1 ]
                [ Html.p [] [ Html.text "Officiis tractatos at sed. Vim ad ipsum ceteros. Posse adolescens ei eos, meliore albucius facilisi id vel, et vel tractatos partiendo. Cu has insolens constituam, sint ubique sit te, vim an legimus elaboraret. Omnes possim mei et. Equidem contentiones vituperatoribus ut vel, duis veri platonem vel ei, an integre consequat democritum qui." ] ]
            , Html.article [ Attr.id "3", Attr.tabindex 1 ]
                [ Html.p [] [ Html.text "Officiis tractatos at sed. Vim ad ipsum ceteros. Posse adolescens ei eos, meliore albucius facilisi id vel, et vel tractatos partiendo. Cu has insolens constituam, sint ubique sit te, vim an legimus elaboraret. Omnes possim mei et. Equidem contentiones vituperatoribus ut vel, duis veri platonem vel ei, an integre consequat democritum qui." ] ]
            ]
    in
    Ui.singleton articles
        ++ (Less.Ui.Html.bounce []
                { here = "#1"
                , label = [ Html.text "Bounce between 1 and 3" ]
                , there = "#3"
                }
                (textLabel "Number Three - this is visible only when `there` is active!")
                |> Ui.at Control
           )


{-| -}
main : Less.Application Features Msg
main =
    Less.application
        { init = init
        , update = update
        , view = view
        }
