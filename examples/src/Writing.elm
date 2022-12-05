module Writing exposing (main)

{-|

@docs Model, main

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Ui exposing (Ui)
import Ui.Application exposing (Application, application)
import Ui.Layout.Aspect exposing (Aspect(..))


{-| -}
type alias Model =
    Int


type Path
    = Page Page
    | NotFound


type Page
    = Home
    | About


init : ( Model, Cmd () )
init =
    ( 0, Cmd.none )


pageFromPath : Ui.Path -> Path
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


view : Ui.Path -> Model -> Ui.Application.Document ()
view rawPath model =
    let
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

        editable : String -> Ui ()
        editable str =
            Html.p [ Attr.contenteditable True ]
                [ Html.text ("Edit me " ++ str)
                ]
                |> Ui.keyed ("Edit me " ++ str)

        path : Path
        path =
            pageFromPath rawPath

        updater : Ui ()
        updater =
            Html.button [ Events.onClick () ]
                [ Html.text ("Update number " ++ String.fromInt model)
                ]
                |> Ui.keyed "updater"
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
        case path of
            Page Home ->
                viewPage "Home"
                    path
                    (Ui.textLabel "This is the home page!")

            Page About ->
                viewPage "About"
                    path
                    (Ui.textLabel "'About' page here!")

            NotFound ->
                viewPage "404"
                    path
                    (Ui.textLabel ("404 Not found: " ++ rawPath))


viewPage : String -> Path -> Ui () -> Ui.Application.Document ()
viewPage title route content =
    { title = title ++ " – SPA"
    , body =
        Ui.constant [ viewNav route ]
            |> Ui.with Scene content
    , layout = Nothing
    }


viewNav : Path -> Html ()
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
main : Application Model ()
main =
    application
        { init = init
        , update = update
        , view = view
        }
