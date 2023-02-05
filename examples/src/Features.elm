module Features exposing (Features, main, Msg)

{-|

@docs Features, main, Msg

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Restrictive exposing (Application, application)
import Restrictive.Layout
import Restrictive.Layout.Region exposing (Aspect(..))
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
-- affects the `current Aspect`!


type alias Ui =
    Ui.Ui
        Aspect
        ( String, Html () )
        (List ( String, Html () ) -> List ( String, Html () ))


type alias Document =
    Restrictive.Document Aspect ( String, Html () ) (List ( String, Html () ) -> List ( String, Html () ))


view : Features -> Document
view _ =
    let
        showTab : String -> Ui -> Ui
        showTab str contents =
            toggle (String.replace " " "-" str)
                |> Ui.with Scene contents
    in
    { body =
        Ui.with Info (Ui.textLabel "Toggle the features on top of the page! ") Ui.singleton
            ++ showTab "Flat Ui Layout"
                ui
            ++ showTab "Global Navbar"
                globalNav
            ++ (Ui.handle [ ( "constant", Html.label [] [ Html.text "ConStAnt" ] ) ]
                    |> Ui.with Scene (Ui.textLabel "ConsScene")
               )
    , layout = Restrictive.Layout.withClass "Features"
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


toggle : String -> Ui
toggle props =
    Restrictive.State.toggle props
        |> Restrictive.State.inlineLink
        |> Ui.custom


goTo : ( Maybe Restrictive.State.Path, Restrictive.State.Fragment ) -> Ui
goTo props =
    Restrictive.State.goTo props
        |> Restrictive.State.inlineLink
        --ViewModel.Change region ( String, Html msg ))
        |> Ui.custom



--(( OrHeader aspect, Url ) -> ViewModel.Change aspect html) -> Ui aspect html wrapper


bounce : { there : ( Maybe Restrictive.State.Path, Restrictive.State.Fragment ), here : ( Maybe Restrictive.State.Path, Restrictive.State.Fragment ) } -> Ui
bounce props =
    Restrictive.State.bounce props
        |> Restrictive.State.inlineLink
        |> Ui.custom


{-| [Application](Ui.Application): Sever Route from Model
-}
paths : Restrictive.State.Path -> Ui
paths path =
    Ui.singleton
        |> Ui.with Scene (Ui.textLabel ("Path: " ++ path))
        |> Ui.with Control (goTo ( Just "Path-1", Nothing ))
        |> Ui.with Control (goTo ( Just "Path-2", Nothing ))
        |> Ui.with Control (goTo ( Just "", Nothing ))
        |> Ui.with Control (goTo ( Nothing, Nothing ))
        |> Ui.with Control (goTo ( Nothing, Just "99" ))


globalNav : Ui
globalNav =
    [ "Introduction", "First Steps", "Last Steps" ]
        |> List.concatMap (\title -> goTo ( Just title, Nothing ))


{-| [Link](Ui.Link): Manage the Ui State as a URL
-}
fragments : Restrictive.State.Fragment -> Ui
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
            (bounce
                { here = ( Nothing, Just "1" ), there = ( Nothing, Just "3" ) }
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
