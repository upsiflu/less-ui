module Features exposing (main)

import Html
import Html.Attributes as Attr
import Less
import Less.Ui
import Less.Ui.Html exposing (layout)
import Markdown exposing (md)


regionExplanation =
    md """
### Screen Regions

Define your regions, then assign your Ui snippets their place on the screen
with `Less.Ui.at`. This is how the welcome text appears in the `Content` region 
and the chapter links in the `Toc`.

```elm 
type Region
    = Toc
    | Content
```
"""


type Region
    = Toc
    | Content


uiExplanation =
    md """
# Ui

```elm
type alias Ui =
    Less.Ui.Html.Ui Region () ()
```

Since a Ui snippet is a list, you can append snippets.
"""


type alias Ui =
    Less.Ui.Html.Ui Region () ()


mdExplanation =
    md """
With `Less.Ui.singleton`, you can turn an Html snippet into a Ui snippet.
This function uses _dillonkearns/elm-markdown_ to render a
multiline string.

```elm
import Markdown.Parser as Markdown
import Markdown.Renderer

md : String -> Ui
md =
    Markdown.parse
        >> ⋯
        >> Less.Ui.singleton
```
"""



---- VIEW ----


viewExplanation =
    md """
# View

```elm
import Less.Ui.Html exposing (layout)
```
"""
        ++ Less.Ui.Html.bounce []
            { there = "View/Body"
            , here = "View"
            , label = [ Html.text "⬍ Body" ]
            }
            bodyExplanation
        ++ Less.Ui.Html.bounce []
            { there = "View/Outline"
            , here = "View"
            , label = [ Html.text "⬍ Outline" ]
            }
            outlineExplanation
        ++ md """
```elm
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
                                |> Html.header [ ... ]

                        toc =
                            Maybe.withDefault [] (rendered.region Toc)
                                |> Html.nav [ ... ]

                        content =
                            Maybe.withDefault [] (rendered.region Content)
                                |> Html.main_ [ ... ]
                    in
                    [ SyntaxHighlight.useTheme SyntaxHighlight.gitHub, header, content, toc ]
        }
    , title = "Less-Ui Walkthrough"
    }
        |> Less.mapDocument identity
```

    """


welcomeExplanation =
    md """

## Welcome!

This is a self-explanatory walkthrough. It presents its own code.
        
Click _Outline_ on the bottom of the screen to open the Table of Contents.

Btw, here is my code:

```elm
welcome : Ui
welcome =
    md  \"\"\"
## Welcome!

This is a self-explanatory walkthrough. It presents its own code.
        
Click _Outline_ below to open the Table of Contents.

And here is my code:

    ```elm
    welcome : Ui
    welcome =
        ∞
    ```
        \"\"\"
        |> Less.Ui.at Content
        |> Less.Ui.Html.goTo []
            { destination = ""
            , isInline = False
            , label = [ Html.h2 [] [ Html.text "⌂" ] ]
            }
```
"""


welcome : Ui
welcome =
    welcomeExplanation
        |> Less.Ui.at Content
        |> Less.Ui.Html.goTo []
            { destination = ""
            , isInline = False
            , label = [ Html.h2 [] [ Html.text "⌂" ] ]
            }


outlineExplanation =
    md """

## Outline

```elm
outline : Ui
outline = 
    [...]
        |> List.concat
        |> Less.Ui.Html.toggle []
            { flag = "❡"
            , isInline = True
            , label = [ Html.b [] [ Html.text "❡ Outline" ] ]
            }
        |> Less.Ui.at Toc
```
        
"""


outline : Ui
outline =
    [ Less.Ui.at Content uiExplanation
        |> Less.Ui.Html.goTo []
            { destination = "Ui"
            , isInline = True
            , label = [ Html.li [] [ Html.text "Ui snippets" ] ]
            }
    , Less.Ui.at Content regionExplanation
        |> Less.Ui.Html.goTo []
            { destination = "Region"
            , isInline = True
            , label = [ Html.li [] [ Html.text "Regions" ] ]
            }
    , Less.Ui.at Content viewExplanation
        |> Less.Ui.Html.goTo []
            { destination = "View"
            , isInline = True
            , label = [ Html.li [] [ Html.text "View and Layout" ] ]
            }
    , Less.Ui.Html.goTo []
        { destination = "View/Body"
        , isInline = True
        , label = [ Html.li [ Attr.style "margin-left" ".8em" ] [ Html.text "Body" ] ]
        }
        []
    , Less.Ui.Html.goTo []
        { destination = "View/Outline"
        , isInline = True
        , label = [ Html.li [ Attr.style "margin-left" ".8em" ] [ Html.text "Outline" ] ]
        }
        []
    , Less.Ui.at Content mainExplanation
        |> Less.Ui.Html.goTo []
            { destination = "Main"
            , isInline = True
            , label = [ Html.li [] [ Html.text "Less Application" ] ]
            }
    , md """
# Filters
            

"""
        |> Less.Ui.at Content
        |> Less.Ui.Html.goTo []
            { destination = "3"
            , isInline = True
            , label = [ Html.li [] [ Html.text "Filters" ] ]
            }
    , md """# Features.elm"""
        ++ regionExplanation
        ++ uiExplanation
        ++ mdExplanation
        ++ viewExplanation
        ++ mainExplanation
        |> Less.Ui.at Content
        |> Less.Ui.Html.goTo []
            { destination = "Module"
            , isInline = True
            , label = [ Html.li [] [ Html.text "The whole module" ] ]
            }
    , md """
## Where to go next:

[https://github.com/upsiflu/less-ui](https://github.com/upsiflu/less-ui)

![under construction](https://upload.wikimedia.org/wikipedia/commons/1/19/Under_construction_graphic.gif)

Have a beautiful day!
          
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
            , label = [ Html.text "❡ Outline" ]
            }
        |> Less.Ui.at Toc


bodyExplanation =
    md """
## Body

```elm
body : Ui
body =
    home ++ outline
```

> _To compose the body of the app, we append two `Ui`s. 
  This is possible because a `Ui` is a `List`._
               """


body : Ui
body =
    welcome ++ outline


view : () -> Less.Document ()
view () =
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
                                |> Html.nav [ Attr.class "❡", Attr.style "position" "fixed", Attr.style "padding" ".5em", Attr.style "background" "silver", Attr.style "right" ".5em", Attr.style "bottom" ".5em" ]

                        content =
                            Maybe.withDefault [] (rendered.region Content)
                                |> Html.main_ [ Attr.style "padding" "0 2.4em 4em 2.4em" ]
                    in
                    [ Markdown.syntaxHighlighting, header, content, toc ]
        }
    , title = "Less-Ui Walkthrough"
    }
        |> Less.mapDocument identity


mainExplanation =
    md """# Main
### _Less code and less control_


A `Less.Application` hides the Ui states in the Url:

```elm
main : Less.Application () ()
main =
    Less.application
        { init = ( (), Cmd.none )
        , update = \\() () -> ( (), Cmd.none )
        , view = view
        }
```

> _As you see, both our `model` and `msg` type are (), meaning the app
defers all state handling to `Less`._"""


{-| -}
main : Less.Application () ()
main =
    Less.application
        { init = ( (), Cmd.none )
        , update = \() () -> ( (), Cmd.none )
        , view = view
        }
