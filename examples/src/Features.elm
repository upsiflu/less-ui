module Features exposing (main)

import Html
import Html.Attributes as Attr
import Less
import Less.Ui
import Less.Ui.Html exposing (layout)


type Region
    = Toc
    | Content


type alias Ui =
    Less.Ui.Html.Ui Region () ()


view : () -> Less.Document ()
view _ =
    let
        chapters : List Ui
        chapters =
            [
             Less.Ui.singleton [ Html.text "Chapter 1" ]
                |> Less.Ui.at Content
                |> Less.Ui.Html.goTo []
                    { destination = "chapter1"
                    , isInline = True
                    , label = [ Html.li [] [ Html.text "GoTo and Toggle" ] ]
                    }

        
             , Less.Ui.singleton [ Html.text "Chapter 2" ]
                |> Less.Ui.at Content
                |> Less.Ui.Html.goTo []
                    { destination = "chapter2"
                    , isInline = True
                    , label = [ Html.li [] [ Html.text "Filters" ] ]
                    }
                    ]

        body : Ui
        body =
            List.concat chapters
                |> Less.Ui.Html.toggle []
                    { flag = "❡"
                    , isInline = True
                    , label = [ Html.b [] [ Html.text "❡ Chapters" ] ]
                    }
                |> Less.Ui.at Toc
    in
    Less.mapDocument identity
        { body = body
        , layout =
            { layout
                | arrange =
                    \rendered ->
                        let
                            header =
                                Maybe.withDefault [] rendered.header
                                    |> Html.header [ Attr.class "header", Attr.style "position" "sticky" ]

                            toc =
                                Maybe.withDefault [] (rendered.region Toc)
                                    |> Html.nav [ Attr.class "❡", Attr.style "position" "fixed", Attr.style "bottom" "1em" ]

                            content =
                                Maybe.withDefault [] (rendered.region Content)
                        in
                        header :: toc :: content
            }
        , title = "Less-Ui feature test"
        }


{-| -}
main : Less.Application () ()
main =
    Less.application
        { init = ( (), Cmd.none )
        , update = \() () -> ( (), Cmd.none )
        , view = view
        }
