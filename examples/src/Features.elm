module Features exposing (main)

import Html
import Html.Attributes as Attr
import Less
import Less.Ui
import Less.Ui.Html exposing (layout)
import Markdown.Parser as Markdown
import Markdown.Renderer
import Result.Extra as Result


type Region
    = Toc
    | Content


type alias Ui =
    Less.Ui.Html.Ui Region () ()


md : String -> Ui
md =
    Markdown.parse
        >> Result.mapError (List.map Markdown.deadEndToString >> String.join "\n")
        >> Result.andThen (Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer)
        >> Result.extract (Html.text >> List.singleton)
        >> Less.Ui.singleton


view : () -> Less.Document ()
view () =
    let
        home : Ui
        home =
            md """
# Welcome!
            
Click "Chapters" to open the Table of Contents.
"""
                |> Less.Ui.at Content
                |> Less.Ui.Html.goTo []
                    { destination = ""
                    , isInline = False
                    , label = [ Html.h2 [] [ Html.text "⌂" ] ]
                    }

        chapters : Ui
        chapters =
            [ md """
# Chapter 1
            
GoTo and Toggle
"""
                |> Less.Ui.at Content
                |> Less.Ui.Html.goTo []
                    { destination = "chapter1"
                    , isInline = True
                    , label = [ Html.li [] [ Html.text "GoTo and Toggle" ] ]
                    }
            , md """
# Chapter 2
            
Filters
"""
                |> Less.Ui.at Content
                |> Less.Ui.Html.goTo []
                    { destination = "chapter2"
                    , isInline = True
                    , label = [ Html.li [] [ Html.text "Filters" ] ]
                    }
            , md """
### Where to go next:

[https://github.com/upsiflu/less-ui](github.com/upsiflu/less-ui)
            
Filters
"""
                |> Less.Ui.at Content
                |> Less.Ui.Html.goTo []
                    { destination = "chapter3"
                    , isInline = True
                    , label = [ Html.li [] [ Html.text "Where to go next" ] ]
                    }
            ]
                |> List.concat
                |> Less.Ui.Html.toggle []
                    { flag = "❡"
                    , isInline = True
                    , label = [ Html.b [] [ Html.text "❡ Chapters" ] ]
                    }
                |> Less.Ui.at Toc

        body : Ui
        body =
            home ++ chapters
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
