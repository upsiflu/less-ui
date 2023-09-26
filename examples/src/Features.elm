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
            
This is a self-explanatory walkthrough. It presents its own code.

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
# A stateless view

```elm

    body : Ui
    body =
        home ++ chapters
```

>>> 💡 _To compose the body of the app, we append two `Ui`s. 
  This is possible because a `Ui` is a `List`._

```elm
    home : Ui
    home =
        md  \"\"\"
            # Welcome!
                    
            Click "Chapters" to open the Table of Contents.
            \"\"\"
            |> Less.Ui.at Content
            |> Less.Ui.Html.goTo []
                { destination = ""
                , isInline = False
                , label = [ Html.h2 [] [ Html.text "⌂" ] ]
                }
```




>>> 💡 _The function `md` renders a markdown string to Html. It's
  omitted here for brevity._

### Screen Regions

Define your regions, then assign your Ui snippets their place on the screen
with `Less.Ui.at`. This is how the welcome text appears in the `Content` region 
and the chapter links in the `Toc`.

```elm
    type Region
        = Toc
        | Content

    chapters : Ui
    chapters = 
        [...]
            |> List.concat
            |> Less.Ui.Html.toggle []
                { flag = "❡"
                , isInline = True
                , label = [ Html.b [] [ Html.text "❡ Chapters" ] ]
                }
            |> Less.Ui.at Toc
```


                
                
                """
                |> Less.Ui.at Content
                |> Less.Ui.Html.goTo []
                    { destination = "View"
                    , isInline = True
                    , label = [ Html.li [] [ Html.text "A stateless view" ] ]
                    }
            , md """
# Layout

```elm
    import Less.Ui.Html exposing (layout)

    ⋯

    view : () -> Less.Document ()
    view () =
        { body = body
        , layout =
            { layout
                | arrange =
                    \\rendered ->
                        let
                            header =
                                Maybe.withDefault [] rendered.header
                                    |> Html.header [ ⋯ ]

                            toc =
                                Maybe.withDefault [] (rendered.region Toc)
                                    |> Html.nav [ ⋯ ]

                            content =
                                Maybe.withDefault [] (rendered.region Content)
                        in
                        header :: content ++ [ toc ]
            }
        , title = "Less-Ui Walkthrough"
        }
            |> Less.mapDocument identity
```

"""
                |> Less.Ui.at Content
                |> Less.Ui.Html.goTo []
                    { destination = "Layout"
                    , isInline = True
                    , label = [ Html.li [] [ Html.text "Layout" ] ]
                    }
            , md """
# Less code and less control


We start with building an application that hides the Ui states in the Url:

```elm
    main : Less.Application () ()
    main =
        Less.application
            { init = ( (), Cmd.none )
            , update = \\() () -> ( (), Cmd.none )
            , view = view
            }
```

>> _As you see, both our `model` and `msg` type are (), meaning the app
defers all state handling to `Less`._
"""
                |> Less.Ui.at Content
                |> Less.Ui.Html.goTo []
                    { destination = "Less"
                    , isInline = True
                    , label = [ Html.li [] [ Html.text "Less Application" ] ]
                    }
            , md """
# Chapter 2
            
GoTo and Toggle
"""
                |> Less.Ui.at Content
                |> Less.Ui.Html.goTo []
                    { destination = "2"
                    , isInline = True
                    , label = [ Html.li [] [ Html.text "GoTo and Toggle" ] ]
                    }
            , md """
# Filters
            
![under construction](https://upload.wikimedia.org/wikipedia/commons/1/19/Under_construction_graphic.gif)
"""
                |> Less.Ui.at Content
                |> Less.Ui.Html.goTo []
                    { destination = "3"
                    , isInline = True
                    , label = [ Html.li [] [ Html.text "Filters" ] ]
                    }
            , md """
### Where to go next:

[https://github.com/upsiflu/less-ui](github.com/upsiflu/less-ui)
          
"""
                |> Less.Ui.at Content
                |> Less.Ui.Html.goTo []
                    { destination = "Next"
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
    { body = body
    , layout =
        { layout
            | arrange =
                \rendered ->
                    let
                        header =
                            Maybe.withDefault [] rendered.header
                                |> Html.header [ Attr.class "header", Attr.style "position" "sticky", Attr.style "top" "0" ]

                        toc =
                            Maybe.withDefault [] (rendered.region Toc)
                                |> Html.nav [ Attr.class "❡", Attr.style "position" "fixed", Attr.style "padding" "1em", Attr.style "background" "#eee", Attr.style "bottom" ".5em" ]

                        content =
                            Maybe.withDefault [] (rendered.region Content)
                                |> Html.main_ [ Attr.style "padding" "0 2.4em 12em 2.4em" ]
                    in
                    [ header, content, toc ]
        }
    , title = "Less-Ui Walkthrough"
    }
        |> Less.mapDocument identity


{-| -}
main : Less.Application () ()
main =
    Less.application
        { init = ( (), Cmd.none )
        , update = \() () -> ( (), Cmd.none )
        , view = view
        }
