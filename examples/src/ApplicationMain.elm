module ApplicationMain exposing (Model, main)

{-|

  - Ui.Application
  - Ui.Layout
  - Ui.Link

@docs Model, main

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed
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
    | Lorem
    | NotFound


init : ( Model, Cmd () )
init =
    ( 0, Cmd.none )


pageFromPath : Link.Path -> Page
pageFromPath path =
    case path of
        "/" ->
            Home

        "/About" ->
            About

        "/DomState" ->
            DomState

        "/Home" ->
            Home

        "/Lorem" ->
            Lorem

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

        Lorem ->
            "Lorem"

        NotFound ->
            "404"


update : () -> Model -> ( Model, Cmd () )
update () model =
    ( model + 1, Cmd.none )


view : ( Link.Path, Link.Fragment ) -> Model -> Ui.Application.Document ()
view ( rawPath, rawFragment ) model =
    let
        page : Page
        page =
            pageFromPath rawPath
    in
    (|>) page <|
        case page of
            Home ->
                viewPage
                    (Ui.textLabel "This is the home page!")

            About ->
                viewPage
                    (Ui.textLabel "'About' page here!"
                        |> Ui.with Info (Ui.textLabel "Check out these links:" ++ Ui.html viewNav ++ myTest)
                    )

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

            Lorem ->
                let
                    viewArticle : String -> List (Html ()) -> Ui ()
                    viewArticle title content =
                        Ui.html (Html.h1 [] [ Html.text ("About " ++ title) ])
                            :: (if title == fragment then
                                    Html.node "center-me" [ Attr.attribute "increment" ("->" ++ fragment) ] []
                                        |> Ui.html
                                        |> Ui.with Control clearFragment

                                else if fragment /= "" then
                                    Html.node "fragment-me" [ Attr.attribute "increment" (title ++ "<-" ++ fragment) ] []
                                        |> Ui.html

                                else
                                    []
                               )
                            :: List.map Ui.html content
                            |> List.concat
                            |> Ui.wrap (Html.Keyed.node "article" [ Attr.id title ] >> Tuple.pair title >> List.singleton)

                    ( fragment, clearFragment ) =
                        case rawFragment of
                            Just f ->
                                ( f
                                , Html.a [ Attr.href ("/" ++ pathFromPage page) ] [ Html.text ("Ignore the " ++ f) ]
                                    |> Ui.html
                                )

                            Nothing ->
                                ( "", [] )
                in
                viewPage
                    (viewArticle "Wolves"
                        [ Html.p [] [ Html.text "Lorem ipsum dolor sit amet, his viris voluptaria ut. Sea ad iusto labitur constituam, viris persius nonumes pro at, detraxit expetendis eu sed. Ut perpetua consequat complectitur sea, eam reque graeci et. Dignissim euripidis intellegat sed ex." ]
                        , Html.a [ Attr.href "#Surprises" ] [ Html.text "👉 Highlight the Surprises 🎉🎉🎉" ]
                        , Html.p [] [ Html.text "Ei legere accumsan sit. Id meis intellegat nec, modo habeo error cum eu. Illud ubique in ius. Meliore nostrum eos an, facilisis reformidans quo in. Ne eruditi assueverit vix, graece eleifend mandamus ut usu. Vis nulla splendide ad." ]
                        ]
                        ++ viewArticle "Canines"
                            [ Html.p [] [ Html.text "Officiis tractatos at sed. Vim ad ipsum ceteros. Posse adolescens ei eos, meliore albucius facilisi id vel, et vel tractatos partiendo. Cu has insolens constituam, sint ubique sit te, vim an legimus elaboraret. Omnes possim mei et. Equidem contentiones vituperatoribus ut vel, duis veri platonem vel ei, an integre consequat democritum qui." ]
                            , Html.a [ Attr.href "#Wolves" ] [ Html.text "👉 Highlight the Wolves 🐺🐺🐺" ]
                            , Html.p [] [ Html.text "Lorem ipsum dolor sit amet, his viris voluptaria ut. Sea ad iusto labitur constituam, viris persius nonumes pro at, detraxit expetendis eu sed. Ut perpetua consequat complectitur sea, eam reque graeci et. Dignissim euripidis intellegat sed ex." ]
                            ]
                        ++ viewArticle "Surprises"
                            [ Html.p [] [ Html.text "Ei legere accumsan sit. Id meis intellegat nec, modo habeo error cum eu. Illud ubique in ius. Meliore nostrum eos an, facilisis reformidans quo in. Ne eruditi assueverit vix, graece eleifend mandamus ut usu. Vis nulla splendide ad." ]
                            , Html.a [ Attr.href "#Canines" ] [ Html.text "👉 Highlight the Canines 🐕🐕🐕" ]
                            , Html.p [] [ Html.text "Officiis tractatos at sed. Vim ad ipsum ceteros. Posse adolescens ei eos, meliore albucius facilisi id vel, et vel tractatos partiendo. Cu has insolens constituam, sint ubique sit te, vim an legimus elaboraret. Omnes possim mei et. Equidem contentiones vituperatoribus ut vel, duis veri platonem vel ei, an integre consequat democritum qui." ]
                            ]
                    )

            NotFound ->
                viewPage
                    (Ui.textLabel ("404 Not found: " ++ rawPath))


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
    [ Home, DomState, Lorem, About ]
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
