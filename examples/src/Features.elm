module Features exposing (main)

import Html
import Html.Attributes as Attr
import Less
import Less.Ui
import Less.Ui.Html exposing (layout)
import Markdown exposing (md)


welcomeExplanation =
    """

# Welcome!

This is a self-explanatory walkthrough. It presents its own code.
        
Click _Outline_ on the bottom of the screen to open the Table of Contents.

Btw, here is my code:

```elm
welcome : Ui
welcome =
    md  \"\"\"
## Welcome!

This is a self-explanatory walkthrough. 
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
    md welcomeExplanation
        |> Less.Ui.at Content
        |> Less.Ui.Html.goTo []
            { destination = ""
            , isInline = False
            , label = [ Html.h2 [] [ Html.text "⌂" ] ]
            }


importExplanation =
    """
# Imports

```elm
import Html
import Html.Attributes as Attr
import Less
import Less.Ui
import Less.Ui.Html exposing (layout)
import Markdown exposing (md)
```
"""


regionExplanation =
    """
# Screen Regions

Define your regions, then assign your Ui snippets their place on the screen
with `#!elm Less.Ui.at`. This is how the welcome text appears in the `#!elm Content` region 
and the chapter links in the `#!elm Toc`.

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
    """
# Ui

```elm
type alias Ui =
    Less.Ui.Html.Ui Region () ()
```

Since a Ui snippet is a list, you can append snippets.
"""


type alias Ui =
    Less.Ui.Html.Ui Region () ()



---- VIEW ----


viewExplanation =
    """
# View

```elm
import Less.Ui.Html exposing (layout)
```
"""
        ++ bodyExplanation
        ++ outlineExplanation
        ++ """
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


outlineExplanation =
    """
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


outline : List String
outline =
    [ importExplanation
    , uiExplanation
    , regionExplanation
    , viewExplanation
    , mainExplanation
    , """
## Where to go next

[https://github.com/upsiflu/less-ui](https://github.com/upsiflu/less-ui)

![under construction](https://upload.wikimedia.org/wikipedia/commons/1/19/Under_construction_graphic.gif)

Have a beautiful day!
          
"""
    ]


createToc : Ui
createToc =
    let
        headings chapter =
            case Markdown.toc chapter of
                Err _ ->
                    []

                Ok results ->
                    List.map .text results

        createLinks chapter =
            case headings chapter of
                [] ->
                    []

                firstHeading :: moreHeadings ->
                    Less.Ui.Html.ul []
                        (atLink (md chapter) ( firstHeading, [] )
                            :: List.map (\subHeading -> atLink [] ( firstHeading, [ subHeading ] )) moreHeadings
                        )

        atLink content ( heading, subheadings ) =
            ( heading
            , Less.Ui.at Content content
                |> Less.Ui.Html.goTo []
                    { destination = String.replace " " "+" (heading ++ "#" ++ Maybe.withDefault heading (List.head subheadings))
                    , isInline = True
                    , label = [ Html.li [] [ Html.text (Maybe.withDefault heading (List.head subheadings)) ] ]
                    }
            )
    in
    List.concatMap createLinks outline
        ++ md "---"
        ++ search
        |> Less.Ui.Html.section []
        |> Less.Ui.Html.toggle []
            { flag = "≔"
            , isInline = True
            , label = [ Html.text "Outline" ]
            }
        |> Less.Ui.at Toc


bodyExplanation =
    """
## Body

---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---
---

```elm
body : Ui
body =
    welcome ++ createToc
```

> _To compose the body of the app, we append two `#!elm Ui`s. 
  This is possible because a `#!elm Ui` is a `#!elm List`._
               """


body : Ui
body =
    welcome ++ createToc


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
                                |> Html.header [ Attr.class "header", Attr.style "position" "sticky", Attr.style "top" "0.5em", Attr.style "max-height" "calc(100vh - 0.5rem)", Attr.style "overflow" "scroll", Attr.style "background" "white" ]

                        toc =
                            Maybe.withDefault [] (rendered.region Toc)
                                |> Html.nav
                                    [ Attr.style "float" "right"
                                    , Attr.style "position" "sticky"
                                    , Attr.style "top" "2rem"
                                    , Attr.style "bottom" "6rem"
                                    , Attr.style "max-width" "20rem"
                                    , Attr.style "transition" "all 2s"

                                    {- Attr.style "position" "fixed", Attr.style "background" "silver", Attr.style "right" ".5em", Attr.style "bottom" ".5em" -}
                                    ]

                        content =
                            Maybe.withDefault [] (rendered.region Content)
                                |> Html.main_ [ Attr.style "padding" "0 2.4em 4em 2.4em" ]
                    in
                    [ Markdown.syntaxHighlighting, header, toc, content ]
        }
    , title = "Less-Ui Walkthrough"
    }
        |> Less.mapDocument identity


searchResultsExplanation =
    """
# Search
```elm
ess.Ui.Html.search [ Attr.placeholder "Find" ]
    { category = "search"
    , isInline = False
    , label = []
    }
    (\\keywords -> 
        ...
    )
```
"""


search : Ui
search =
    let
        results keywords =
            outline
                |> List.filterMap
                    (\explanation ->
                        case
                            List.filter
                                (\keyword -> keyword /= "" && String.contains keyword explanation)
                                keywords
                        of
                            [] ->
                                Nothing

                            matches ->
                                Just ( matches, explanation )
                    )
                >> List.sortBy (Tuple.first >> List.length >> negate)
                >> List.concatMap
                    (\( keywordsFound, explanationFound ) ->
                        case Markdown.toc explanationFound of
                            Ok toc ->
                                List.concatMap
                                    (\{ text } ->
                                        Less.Ui.Html.toggle []
                                            { flag = String.replace " " "+" text
                                            , isInline = True
                                            , label = [ Html.text (text ++ " (" ++ String.join ", " keywordsFound ++ ")") ]
                                            }
                                            (md explanationFound)
                                            ++ md "---"
                                    )
                                    toc

                            Err message ->
                                message
                    )
    in
    Less.Ui.Html.search [ Attr.placeholder "Find" ]
        { category = "search"
        , isInline = True
        , label = []
        }
        (\keywordList ->
            case List.concatMap (String.split " " >> List.filter ((/=) "")) keywordList of
                [] ->
                    []

                keywords ->
                    md "**Search Results:**" ++ results keywords
        )


mainExplanation =
    """# Main
### _Less code and less control_


A `#!elm Less.Application` hides the Ui states in the Url:

```elm
main : Less.Application () ()
main =
    Less.application
        { init = ( (), Cmd.none )
        , update = \\() () -> ( (), Cmd.none )
        , view = view
        }
```

> _As you see, both our `#!elm model` and `#!elm msg` type are (), meaning the app
defers all state handling to `#!elm Less`._"""


{-| -}
main : Less.Application () ()
main =
    Less.application
        { init = ( (), Cmd.none )
        , update = \() () -> ( (), Cmd.none )
        , view = view
        }
