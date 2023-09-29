module Features exposing (main)

import Html
import Html.Attributes as Attr
import Less
import Less.Ui
import Less.Ui.Html exposing (layout)
import Markdown exposing (md)


welcomeExplanation : String
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


importExplanation : String
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


regionExplanation : String
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


uiExplanation : String
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


viewExplanation : String
viewExplanation =
    """
# View

```elm
import Less.Ui.Html exposing (layout)
```
"""
        ++ bodyExplanation
        ++ outlineExplanation
        ++ tokenExplanation
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


outlineExplanation : String
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


tokenExplanation : String
tokenExplanation =
    """
# Tokens

[Toggle Tokens](?toggle=showTokens)

```elm
Toggle "showTokens" [ Search "q" [List.concatMap viewToken >> at Toc] ]
```

    """


createToc : Ui
createToc =
    let
        createChapterLinks attrs chapter =
            case Markdown.toc chapter of
                Err _ ->
                    md "(Markdown parsing error)"

                Ok [] ->
                    md "(No headings)"

                Ok (firstHeading :: moreHeadings) ->
                    Less.Ui.Html.section
                        attrs
                        (createLink (md chapter) ( firstHeading.text, [] )
                            ++ List.concatMap (\subHeading -> createLink [] ( firstHeading.text, [ subHeading.text ] )) moreHeadings
                        )

        createLink content ( heading, subheadings ) =
            Less.Ui.at Content content
                |> Less.Ui.Html.goTo [ Attr.style "padding" "1ch", Attr.style "display" "inline-block", Attr.style "background" "#ddd" ]
                    { destination =
                        heading
                            ++ "#"
                            ++ Maybe.withDefault heading (List.head subheadings)
                            |> String.replace " " "+"
                    , isInline = True
                    , label = [ Html.text (Maybe.withDefault heading (List.head subheadings)) ]
                    }

        outline : List String
        outline =
            [ importExplanation
            , uiExplanation
            , regionExplanation
            , viewExplanation
            , mainExplanation
            , whereToGoNext
            ]
    in
    Less.Ui.Html.search [ Attr.placeholder "Search" ]
        { category = "search"
        , isInline = True
        , label = []
        }
        (\searchTerms ->
            let
                hasAllSearchTerms : String -> Bool
                hasAllSearchTerms chapter =
                    List.foldl
                        (\searchTerm -> String.contains searchTerm chapter |> (&&))
                        True
                        searchTerms

                tokens : Ui
                tokens =
                    List.filter ((/=) "") searchTerms
                        |> List.concatMap
                            (\searchTerm ->
                                Less.Ui.Html.toggle []
                                    { flag = "search=" ++ String.join " " (List.filter ((/=) searchTerm) searchTerms)
                                    , isInline = True
                                    , label = [ Html.button [] [ Html.text (searchTerm ++ " ✖") ] ]
                                    }
                                    []
                            )
                        |> Less.Ui.Html.toggle []
                            { flag = "showTokens"
                            , isInline = True
                            , label = []
                            }
            in
            List.concatMap
                (\chapter ->
                    if hasAllSearchTerms chapter then
                        createChapterLinks [ Attr.style "transition" "all .2s" ] chapter

                    else
                        createChapterLinks [ Attr.style "transition" "all .2s", Attr.style "font-size" "0" ] chapter
                )
                outline
                ++ tokens
        )
        |> Less.Ui.at Toc


bodyExplanation : String
bodyExplanation =
    """
## Body

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
                \renderedHtml ->
                    let
                        header =
                            Maybe.withDefault [] renderedHtml.header
                                |> Html.header
                                    [ Attr.class "header"
                                    , Attr.style "position" "sticky"
                                    , Attr.style "top" "0.5em"
                                    ]

                        toc =
                            Maybe.withDefault [] (renderedHtml.region Toc)
                                |> Html.nav
                                    -- [ Attr.style "float" "right"
                                    -- , Attr.style "position" "sticky"
                                    -- , Attr.style "top" "2rem"
                                    -- , Attr.style "bottom" "6rem"
                                    -- , Attr.style "max-width" "20rem"
                                    -- , Attr.style "transition" "all 2s"
                                    -- ]
                                    [ Attr.style "display" "inline-block"
                                    , Attr.style "position" "sticky"
                                    , Attr.style "top" "3rem"
                                    , Attr.style "vertical-align" "top"
                                    , Attr.style "margin" "0rem 3rem 0 0"
                                    ]

                        content =
                            Maybe.withDefault [] (renderedHtml.region Content)
                                |> Html.main_
                                    -- [ Attr.style "padding" "0 2.4em 4em 2.4em" ]
                                    [ Attr.style "display" "inline-block"

                                    -- , Attr.style "padding" "0 2.4em 4em 2.4em"
                                    ]
                    in
                    [ Markdown.syntaxHighlighting, header, toc, content ]
        }
    , title = "Less-Ui Walkthrough"
    }
        |> Less.mapDocument identity


mainExplanation : String
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


whereToGoNext : String
whereToGoNext =
    """
## Where to go next

[https://github.com/upsiflu/less-ui](https://github.com/upsiflu/less-ui)

![under construction](https://upload.wikimedia.org/wikipedia/commons/1/19/Under_construction_graphic.gif)

Have a beautiful day!
        
    """
