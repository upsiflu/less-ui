module ApplicationMain exposing (Model, main)

{-|

@docs Model, main

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Ui exposing (Ui)
import Ui.Application exposing (Application, application)
import Ui.Layout as Layout
import Ui.Layout.Aspect exposing (Aspect(..))
import Ui.Link as Link


{-| -}
type alias Model =
    Int


type Page
    = Home
    | About
    | DomState
    | NotFound


init : ( Model, Cmd () )
init =
    ( 0, Cmd.none )


pageFromPath : Link.Path -> Page
pageFromPath path =
    case path of
        "/" ->
            Home

        "/Home" ->
            Home

        "/DomState" ->
            DomState

        "/About" ->
            About

        _ ->
            NotFound


pathFromPage : Page -> Link.Path
pathFromPage page =
    case page of
        Home ->
            "Home"

        About ->
            "About"

        DomState ->
            "DomState"

        NotFound ->
            "404"


update : () -> Model -> ( Model, Cmd () )
update () model =
    ( model + 1, Cmd.none )



--view : Path -> model -> Document modelMsg


view : ( Link.Path, Link.Fragment ) -> Model -> Ui.Application.Document ()
view ( rawPath, _ ) model =
    case pageFromPath rawPath of
        Home ->
            viewPage
                (Ui.textLabel "This is the home page!")
                Home

        About ->
            viewPage
                (Ui.textLabel "'About' page here!"
                    |> Ui.with Info (Ui.textLabel "Check out these links:" ++ Ui.html viewNav ++ myTest)
                )
                About

        DomState ->
            let
                counter : Ui ()
                counter =
                    List.repeat model ()
                        |> List.indexedMap
                            (\i _ ->
                                if modBy 2 i == 0 then
                                    editable -1 ("I will be reset (" ++ String.fromInt i ++ ")")

                                else
                                    editable i ("I store your text in the DOM in position " ++ String.fromInt i ++ "")
                            )
                        |> List.concat
                        |> List.reverse
                        |> Ui.ul "counter"

                editable : Int -> String -> Ui ()
                editable key str =
                    Html.p [ Attr.contenteditable True, Attr.title ("Keyed with: " ++ String.fromInt key) ] [ Html.text str ]
                        |> Ui.keyed (String.fromInt key)

                updater : Ui ()
                updater =
                    Html.button [ Events.onClick () ]
                        [ Html.text ("Add item #" ++ String.fromInt model) ]
                        |> Ui.keyed "updater"
            in
            viewPage
                (Ui.textLabel "Play with a stateful Dom :-D"
                    |> Ui.with Control updater
                    |> Ui.with Scene counter
                    |> Ui.with Info (Ui.textLabel "Add boxes to the list and edit their labels. Some boxes retain their data when you add new ones. Note that Elm only stores the number of boxes. All labels are stored by your browser in the DOM. This data will be lost when you navigate through the pages.")
                )
                About

        NotFound ->
            viewPage
                (Ui.textLabel ("404 Not found: " ++ rawPath))
                NotFound


myTest : Ui ()
myTest =
    Ui.textLabel "The right order: "
        ++ Ui.textLabel "1"
        ++ Ui.textLabel "2"
        ++ Ui.textLabel "3"
        |> Ui.with Info (Ui.textLabel "a" ++ Ui.textLabel "b")


viewPage : Ui () -> Page -> Ui.Application.Document ()
viewPage content page =
    { body =
        Ui.constant [ viewNav ]
            |> Ui.with Scene content
    , layout = Layout.Default
    , title = pathFromPage page ++ " – SPA"
    }


viewNav : Html ()
viewNav =
    [ Home, DomState, About ]
        |> List.map
            (\page ->
                Html.li []
                    [ Html.a
                        [ Attr.href (pathFromPage page) ]
                        [ Html.text (pathFromPage page) ]
                    ]
            )
        |> Html.ul []
        |> List.singleton
        |> Html.nav []


{-| -}
main : Application Model ()
main =
    application
        { init = init
        , update = update
        , view = view
        }
