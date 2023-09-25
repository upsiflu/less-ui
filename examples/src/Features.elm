module Features exposing (main)

import Html exposing (Html)
import Html.Attributes as Attr
import Less
import Less.Ui
import Less.Ui.Html exposing (layout)
import Less.Ui.Region exposing (OrHeader(..))


type Region
    = Toc
    | Main


type alias Ui =
    Less.Ui.Html.Ui Region () ()


view : () -> Less.Document ()
view _ =
    let
        chapter1 : Ui
        chapter1 =
            Less.Ui.singleton [ Html.text "Chapter 1" ]
                |> Less.Ui.at Main
                |> Less.Ui.Html.goTo []
                    { destination = "chapter1"
                    , isInline = True
                    , label = [ Html.li [] [ Html.text "Chapter 1: GoTo and Toggle" ] ]
                    }

        chapter2 : Ui
        chapter2 =
            Less.Ui.singleton [ Html.text "Chapter 2" ]
                |> Less.Ui.at Main
                |> Less.Ui.Html.goTo []
                    { destination = "chapter2"
                    , isInline = True
                    , label = [ Html.li [] [ Html.text "Chapter 2: GoTo and Toggle" ] ]
                    }

        body : Ui
        body =
            Less.Ui.Html.toggle []
                { flag = "Toc"
                , isInline = True
                , label = [ Html.b [] [ Html.text "Table of Contents" ] ]
                }
                (chapter1 ++ chapter2)
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

                            toc___ =
                                Maybe.withDefault [] (rendered.region Toc)
                                    |> Html.nav [ Attr.class "toc", Attr.style "position" "fixed", Attr.style "bottom" "0" ]

                            main__ =
                                Maybe.withDefault [] (rendered.region Main)
                        in
                        header :: toc___ :: main__
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
