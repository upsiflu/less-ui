module ApplicationMain exposing (Model, main)

{-|

@docs Model, main

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Ui exposing (Ui)
import Ui.Layout.Aspect exposing (Aspect(..))


{-| -}
type alias Model =
    Int


type Route
    = Page Page
    | NotFound


type Page
    = Home
    | About


init : ( Model, Cmd () )
init =
    ( 0, Cmd.none )


pageFromPath : Ui.Path -> Route
pageFromPath path =
    case path of
        "/" ->
            Page Home

        "/About" ->
            Page About

        "/Home" ->
            Page Home

        _ ->
            NotFound


pathFromPage : Page -> Ui.Path
pathFromPage page =
    case page of
        Home ->
            "/Home"

        About ->
            "/About"


update : () -> Model -> ( Model, Cmd () )
update () model =
    ( model + 1, Cmd.none )



--view : Path -> model -> Document modelMsg


view : Ui.Path -> Model -> Ui.Document ()
view path model =
    let
        route : Route
        route =
            pageFromPath path

        updater : Ui ()
        updater =
            Html.button [ Events.onClick () ]
                [ Html.text ("Update number " ++ String.fromInt model)
                ]
                |> Ui.keyed "updater"

        editable : String -> Ui ()
        editable str =
            Html.p [ Attr.contenteditable True ]
                [ Html.text ("Edit me " ++ str)
                ]
                |> Ui.keyed ("Edit me " ++ str)

        appendCounter : Ui () -> Ui ()
        appendCounter =
            List.repeat model ()
                |> List.indexedMap
                    (\i _ ->
                        let
                            number : String
                            number =
                                String.fromInt i
                        in
                        Ui.textLabel number
                            ++ editable ("(incrementing key " ++ number ++ ")")
                            ++ editable "(constant key)"
                            |> Ui.with Scene
                    )
                |> List.foldl (<<) identity
    in
    (\doc ->
        { doc
            | body =
                doc.body
                    |> Ui.with Control updater
                    |> appendCounter
        }
    )
    <|
        case route of
            Page Home ->
                viewPage "Home"
                    route
                    (Ui.textLabel "This is the home page!")

            Page About ->
                viewPage "About"
                    route
                    (Ui.textLabel "'About' page here!")

            NotFound ->
                viewPage "404"
                    route
                    (Ui.textLabel ("404 Not found: " ++ path))


viewPage : String -> Route -> Ui () -> Ui.Document ()
viewPage title route content =
    { title = title ++ " – SPA"
    , body =
        Ui.constant [ viewNav route ]
            |> Ui.with Scene content
    , layout = Nothing
    }


viewNav : Route -> Html ()
viewNav maybePage =
    let
        items =
            [ ( Home, "Home" )
            , ( About, "About" )
            ]
    in
    Html.nav []
        [ Html.ul []
            (items
                |> List.map
                    (\( page, text ) ->
                        Html.li []
                            [ Html.a
                                (if Page page == maybePage then
                                    []

                                 else
                                    [ Attr.href (pathFromPage page)
                                    ]
                                )
                                [ Html.text text ]
                            ]
                    )
            )
        ]


{-| -}
main : Ui.Application Model ()
main =
    Ui.application
        { init = init
        , update = update
        , view = view
        }
