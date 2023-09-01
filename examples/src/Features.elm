module Features exposing (Features, main, Msg)

{-|

@docs Features, main, Msg

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Restrictive
import Restrictive.Layout.Html.Keyed as Keyed exposing (Wrapper(..))
import Restrictive.Layout.Region exposing (Region(..))
import Restrictive.State
import Restrictive.Ui as Ui


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
    Keyed.Ui Msg Msg


textLabel : String -> Ui
textLabel str =
    Ui.singleton [ ( str, Html.text str ) ]


view : Features -> Restrictive.Document Msg
view _ =
    let
        showTab : String -> Ui -> Ui
        showTab str contents =
            Keyed.toggle []
                { flag = str
                , isInline = False
                , label = [ ( str, Html.text str ) ]
                }
                contents
    in
    Restrictive.mapDocument Keyed.toHtml
        { body =
            textLabel "Toggle the features on top of the page! "
                ++ showTab "Flat Ui Layout"
                    ui
                ++ showTab "Global Navbar"
                    globalNav
        , layout = Keyed.layout
        , title = "Restrictive Ui feature test"
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
    Keyed.goTo []
        { destination = ( Just "Path-1", Nothing )
        , isInline = True
        , label = [ ( "label", Html.text "Path-1" ) ]
        }
        []
        ++ Keyed.goTo []
            { destination = ( Just "Path-2", Nothing )
            , isInline = True
            , label = [ ( "label", Html.text "Path-2" ) ]
            }
            []
        ++ Keyed.goTo []
            { destination = ( Nothing, Nothing )
            , isInline = True
            , label = [ ( "label", Html.text "Nothing" ) ]
            }
            []


globalNav : Ui
globalNav =
    [ "Introduction", "First Steps", "Last Steps" ]
        |> List.concatMap
            (\title ->
                Keyed.goTo []
                    { destination = ( Just title, Nothing )
                    , isInline = False
                    , label = [ ( title, Html.text title ) ]
                    }
                    (textLabel title)
            )


{-| [Link](Ui.Link): Manage the Ui State as a URL
-}
fragments : Restrictive.State.Fragment -> Ui
fragments fr =
    let
        articles : List ( String, Html msg )
        articles =
            [ Html.article [ Attr.id "1", Attr.tabindex 1 ]
                [ Html.p [] [ Html.text "Officiis tractatos at sed. Vim ad ipsum ceteros. Posse adolescens ei eos, meliore albucius facilisi id vel, et vel tractatos partiendo. Cu has insolens constituam, sint ubique sit te, vim an legimus elaboraret. Omnes possim mei et. Equidem contentiones vituperatoribus ut vel, duis veri platonem vel ei, an integre consequat democritum qui." ] ]
            , Html.article [ Attr.id "2", Attr.tabindex 1 ]
                [ Html.p [] [ Html.text "Officiis tractatos at sed. Vim ad ipsum ceteros. Posse adolescens ei eos, meliore albucius facilisi id vel, et vel tractatos partiendo. Cu has insolens constituam, sint ubique sit te, vim an legimus elaboraret. Omnes possim mei et. Equidem contentiones vituperatoribus ut vel, duis veri platonem vel ei, an integre consequat democritum qui." ] ]
            , Html.article [ Attr.id "3", Attr.tabindex 1 ]
                [ Html.p [] [ Html.text "Officiis tractatos at sed. Vim ad ipsum ceteros. Posse adolescens ei eos, meliore albucius facilisi id vel, et vel tractatos partiendo. Cu has insolens constituam, sint ubique sit te, vim an legimus elaboraret. Omnes possim mei et. Equidem contentiones vituperatoribus ut vel, duis veri platonem vel ei, an integre consequat democritum qui." ] ]
            ]
                |> List.indexedMap (String.fromInt >> Tuple.pair)
    in
    Ui.singleton articles
        ++ (Keyed.bounce []
                { here = ( Nothing, Just "1" )
                , label = [ ( "label", Html.text "Bounce between 1 and 3" ) ]
                , there = ( Nothing, Just "3" )
                }
                (textLabel "Number Three - this is visible only when `there` is active!")
                |> Ui.at Control
           )


{-| -}
main : Restrictive.Application Features Msg
main =
    Restrictive.application
        { init = init
        , update = update
        , view = view
        }
