module Features exposing (Features, main, Msg)

{-|

@docs Features, main, Msg

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Ui
import Ui.Application exposing (Application, application)
import Ui.Layout exposing (Layout)
import Ui.Layout.Aspect exposing (Aspect(..))
import Ui.Link as Link
import Ui.State


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
-- affects the `current Aspect`!


type alias Ui =
    Ui.Ui Aspect ( String, Html () ) (List ( String, Html () ) -> List ( String, Html () ))


type alias Document =
    Ui.Application.Document Aspect ( String, Html () ) (List ( String, Html () ) -> List ( String, Html () ))


view : ( Ui.State.Path, Ui.State.Fragment ) -> Features -> Document
view ( path, fragment ) _ =
    let
        showTab : String -> Ui -> Ui
        showTab str contents =
            Link.toggle (String.replace " " "-" str)
                |> Link.view (Link.preset.global [] [ Html.text str ])
                |> Ui.with Scene contents
    in
    { body =
        Ui.with Info (Ui.textLabel "Toggle the features on top of the page! ") Ui.singleton
            ++ showTab "Flat Ui Layout"
                ui
            ++ showTab "Global Navbar"
                globalNav
            ++ showTab "Path in View"
                (paths path)
            ++ showTab "Bounce between fragments"
                (fragments fragment)
            ++ (Ui.handle [ ( "constant", Html.label [] [ Html.text "ConStAnt" ] ) ]
                    |> Ui.with Scene (Ui.textLabel "ConsScene")
               )
    , layout = Ui.Layout.withClass "Features"
    , title = "Restrictive Ui feature test"
    }


{-| [Ui](Ui): Flat layout instead of nested components
-}
ui : Ui
ui =
    Ui.handle [ ( "handle", Html.label [] [ Html.text "Handle" ] ) ]
        |> Ui.with Scene (Ui.textLabel "Scene")
        |> Ui.with Control (Ui.textLabel "Control")
        |> Ui.with Info (Ui.textLabel "Info")


{-| [Application](Ui.Application): Sever Route from Model
-}
paths : Ui.State.Path -> Ui
paths path =
    Ui.singleton
        |> Ui.with Scene (Ui.textLabel ("Path: " ++ path))
        |> Ui.with Control (Link.goTo ( Just "Path-1", Nothing ) |> Link.view (Link.preset.inline [ Attr.class "paths" ] [ Html.text "Go to Path 1" ]))
        |> Ui.with Control (Link.goTo ( Just "Path-2", Nothing ) |> Link.view (Link.preset.inline [ Attr.class "paths" ] [ Html.text "Go to Path 2" ]))
        |> Ui.with Control (Link.goTo ( Just "", Nothing ) |> Link.view (Link.preset.inline [ Attr.class "paths" ] [ Html.text "Go home" ]))
        |> Ui.with Control (Link.goTo ( Nothing, Nothing ) |> Link.view (Link.preset.inline [ Attr.class "paths" ] [ Html.text "Go nowhere" ]))
        |> Ui.with Control (Link.goTo ( Nothing, Just "99" ) |> Link.view (Link.preset.inline [ Attr.class "paths" ] [ Html.text "Go to Fragment #99" ]))


globalNav : Ui
globalNav =
    [ "Introduction", "First Steps", "Last Steps" ]
        |> List.concatMap (\title -> Link.goTo ( Just title, Nothing ) |> Link.view (Link.preset.nav [] []))


{-| [Link](Ui.Link): Manage the Ui State as a URL
-}
fragments : Ui.State.Fragment -> Ui
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
    Ui.singleton
        |> Ui.with Control
            (Link.bounce
                { here = ( Nothing, Just "1" ), there = ( Nothing, Just "3" ) }
                |> Link.view (Link.preset.inline [ Attr.class "paths" ] [ Html.text "bounce between 1 and 3" ])
            )
        |> Ui.with Scene
            (Ui.html ( "articles", Html.section [] articles ))
        |> Ui.with Info (Ui.textLabel (Maybe.withDefault "(No fragment)" fr))


{-| -}
main : Application Features Msg
main =
    application
        { init = init
        , update = update
        , view = view
        }
