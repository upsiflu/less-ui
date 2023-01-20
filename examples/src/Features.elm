module Features exposing
    ( main, Msg
    , ui, paths, fragments
    )

{-|

@docs main, Msg

@docs ui, paths, fragments

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Ui exposing (Ui)
import Ui.Application exposing (Application, application)
import Ui.Layout exposing (Layout)
import Ui.Layout.Aspect exposing (Aspect(..))
import Ui.Link as Link
import Ui.State


{-| -}
type alias Msg =
    ()


type alias Features =
    {}


init : ( Features, Cmd Msg )
init =
    ( {}, Cmd.none )


update : Msg -> Features -> ( Features, Cmd Msg )
update () features =
    ( features, Cmd.none )


view : ( Ui.State.Path, Ui.State.Fragment ) -> Features -> { body : Ui Msg, layout : Layout, title : String }
view ( path, fragment ) features =
    let
        showTab : String -> Ui Msg -> Ui Msg
        showTab str contents =
            Link.toggle (String.replace " " "-" str)
                |> Link.view (Link.preset.global [] [ Html.text str ])
                |> Ui.with Control contents
    in
    { title = "Restrictive Ui feature test"
    , layout = Ui.Layout.WithClass "Features"
    , body =
        Ui.with Info (Ui.textLabel "Toggle the features on top of the page! ") Ui.singleton
            ++ showTab "Flat Ui Layout" ui
            ++ showTab "Path in View"
                (paths path)
            ++ showTab "Bounce between fragments"
                (fragments fragment)
    }


{-| [Ui](Ui): Flat layout instead of nested components
-}
ui : Ui Msg
ui =
    Ui.constant [ Html.label [] [ Html.text "Handle" ] ]
        |> Ui.with Scene (Ui.textLabel "Scene")
        |> Ui.with Control (Ui.textLabel "Control")
        |> Ui.with Info (Ui.textLabel "Info")


{-| [Application](Ui.Application): Sever Route from Model
-}
paths : Ui.State.Path -> Ui Msg
paths path =
    Ui.singleton
        |> Ui.with Scene (Ui.textLabel ("Path: " ++ path))
        |> Ui.with Control (Link.goTo ( "Path-1", Nothing ) |> Link.view (Link.preset.inline [] [ Html.text "Go to Path 1" ]))
        |> Ui.with Control (Link.goTo ( "Path-2", Nothing ) |> Link.view (Link.preset.inline [] [ Html.text "Go to Path 2" ]))


{-| [Link](Ui.Link): Manage the Ui State as a URL
-}
fragments : Ui.State.Fragment -> Ui Msg
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
                { there = ( "", Just "3" ), here = ( "", Just "1" ) }
                |> Link.view (Link.preset.inline [] [ Html.text "bounce between 1 and 3" ])
            )
        |> Ui.with Scene
            (Ui.html (Html.section [] articles))
        |> Ui.with Info (Ui.textLabel (Maybe.withDefault "(No fragment)" fr))


{-| -}
main : Application Features Msg
main =
    application
        { init = init
        , update = update
        , view = view
        }
