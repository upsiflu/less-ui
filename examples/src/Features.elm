module Features exposing (main)

import Html
import Html.Attributes as Attr
import Less
import Less.Ui as Ui
import Less.Ui.Html as Ui exposing (layout)
import Markdown exposing (md)


welcomeExplanation : String
welcomeExplanation =
    """

# Welcome!

This is a self-explaning walkthrough. It presents its own code.

Open [the Elm module](https://github.com/upsiflu/less-ui/blob/main/examples/src/Features.elm) in your editor
of choice and follow along!

Btw, here is the code that created this message:

```elm
viewWelcome : Ui
viewWelcome =
    md  \"\"\"∞\"\"\"
        |> Less.Ui.inRegion Content
        |> Less.Ui.Html.goTo []
            { destination = ""
            , inHeader = True
            , label = [ Html.h2 [] [ Html.text "⌂" ] ]
            }
```
"""


viewWelcome : Ui
viewWelcome =
    md welcomeExplanation
        |> Ui.inRegion Content
        |> Ui.goTo []
            { destination = ""
            , inHeader = True
            , label = [ Html.h2 [] [ Html.text "⌂" ] ]
            }


type Region
    = Toc
    | Content


uiExplanation : String
uiExplanation =
    """
# Ui

```elm
import Html
import Html.Attributes as Attr
import Less
import Less.Ui
import Less.Ui.Html exposing (layout)
import Markdown exposing (md)
```

An Html Ui has three type parameters:
- The [Screen Region](Screen+Regions) of a snipped sets where on the screen it will appear.
For example, `#!elm ui |> inRegion X` moves the Ui to region "X". 
[Check out `#!elm View` for how regions are laid out on the screen.](View#Layout)
- The second parameter is the message type. This app is completely Url-driven and has
neither a model nor any messages, so the message type is "unit" `#!elm ()`.
- The third parameter is the message type of nested Html. It's only relevant if you want
to nest Ui code inside a function that transforms from Html a to Html b. For example with
`elm-any-type-forms`.

```elm
type alias Ui =
    Less.Ui.Html.Ui Region () ()
```
"""


type alias Ui =
    Ui.Html Region () ()


regionExplanation : String
regionExplanation =
    """
# Screen Regions

Define your regions, then assign your Ui snippets their place on the screen
with `#!elm Less.Ui.inRegion`. This is how this text appears in the `#!elm Content` region 
and the chapter links in the `#!elm Toc`.

```elm 
type Region
    = Toc
    | Content
```

>> _Read about how the regions are laid out on the screen in [the `View` chapter](View)._
"""



---- VIEW ----


viewExplanation : String
viewExplanation =
    """
# View


```elm
body : Ui
body =
    viewWelcome ++ viewToc
```

>> _To compose the body of the app, we append two `#!elm Ui`s. 
  This is possible because a `#!elm Ui` is a `#!elm List`._
               

```elm
outline : List String
outline =
    [ uiExplanation
    , regionExplanation
    , viewExplanation
    , mainExplanation
    , whereToGoNext
    ]
```

>> _These are the `Markdown` snippets you are reading right now. 
The `#!elm view` takes this content and transforms it into a list 
of `#!elm goTo` links with associated routes and pages._


## Search and Filter

```elm
viewToc : Ui
viewToc =
    Less.Ui.Html.search [ Attr.placeholder "Search" ]
        { category = "search"
        , inHeader = False
        , label = []
        }
        searchToc
        |> Less.Ui.inRegion Toc
```


>> _The `#!elm search` pattern produces an input field where users can enter space-separated search terms.
In `#!elm searchToc`, we respond dynamically to these changes._

```elm
searchToc : List String -> Ui
searchToc searchTerms =
    let
```
>> _For each chapter, draw a link, then hide it with a graceful transition if it doesn't contain all search terms._
```elm
    in
    List.concatMap viewChapter outline
        ++ tokens
```

## Tokens

To show a token for each search term, we use a trick:
A `#!elm Less` application accepts several assignments (`#!elm ?a=b`) in the Url query. In the `#!elm search` and
`#!elm filter` patterns, the nested Ui receives the intersection of all assignments that match the `#!elm Category`
being filtered, in our case `#!elm search`.

This means that if the current assignment is `#!elm search=a b c`, I can add a flag `#!elm search=a b` to
filter out `#!elm c`:

```elm
Less.Ui.Html.toggle []
    { flag = 
        "search=" 
            ++ String.join 
                " " 
                (List.filter ((/=) searchTerm) searchTerms)
    , inHeader = False
    , label = [ Html.button [] [ Html.text (searchTerm ++ " ×") ] ]
    }
    []
```

Enter some search terms, then click the following link to toggle the visibility of search term tokens!

[Toggle me the tokens](?toggle=showTokens)

The markdown for this Link is `[Toggle me the tokens](?toggle=showTokens)`,
and here is the `#!elm toggle` pattern that hides and shows the toggles:

```elm
Less.Ui.Html.toggle []
    { flag = "showTokens"
    , inHeader = False
    , label = []
    }
    -- Here come the toggles (as long as they are toggled on)
```

# Layout

We are extending `#!elm Less.Ui.Html.layout`,
using a custom `#!elm arrange` function that receives the rendered Html snippets per screen region.

```elm
view : () -> Less.Document ()
view () =
    { body = body
    , layout =
        { layout 
            | arrange =
                \\renderedHtml ->
                    let
                        header =
                            Maybe.withDefault [] renderedHtml.header
                                |> Html.header [ ... ]

                        toc =
                            Maybe.withDefault [] (renderedHtml.region Toc)
                                |> Html.nav [ ... ]

                        content =
                            Maybe.withDefault [] (renderedHtml.region Content)
                                |> Html.main_ [ ... ]
                    in
                    [ Markdown.syntaxHighlighting, header, toc, content ]
        }
    , title = "Less-Ui Walkthrough"
    }
        |> Less.mapDocument identity
```


    """


outline : List String
outline =
    [ uiExplanation
    , regionExplanation
    , viewExplanation
    , mainExplanation
    , whereToGoNext
    ]


searchToc : List String -> Ui
searchToc searchTerms =
    let
        addHeadingLinks : ( String, List String ) -> Ui -> Ui
        addHeadingLinks ( heading, subheadings ) =
            Ui.inRegion Content
                >> Ui.goTo [ Attr.style "padding" ".5ch", Attr.style "display" "inline-block", Attr.style "background" "#ddd" ]
                    { destination =
                        heading
                            ++ "#"
                            ++ Maybe.withDefault heading (List.head subheadings)
                            |> String.replace " " "+"
                    , inHeader = False
                    , label = [ Html.text (Maybe.withDefault heading (List.head subheadings)) ]
                    }

        viewChapter : String -> Ui
        viewChapter chapter =
            case Markdown.toc chapter of
                Err _ ->
                    md "(Markdown parsing error)"

                Ok [] ->
                    md "(No headings)"

                Ok (firstHeading :: moreHeadings) ->
                    addHeadingLinks ( firstHeading.text, [] ) (md chapter)
                        ++ List.concatMap (\subHeading -> addHeadingLinks ( firstHeading.text, [ subHeading.text ] ) []) moreHeadings
                        |> Ui.section
                            (if hasAllSearchTerms chapter then
                                [ Attr.style "transition" "all .2s" ]

                             else
                                [ Attr.style "transition" "all .2s", Attr.style "font-size" "0" ]
                            )

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
                        Ui.toggle []
                            { flag = "search=" ++ String.join " " (List.filter ((/=) searchTerm) searchTerms)
                            , inHeader = False
                            , label = [ Html.button [] [ Html.text (searchTerm ++ " ×") ] ]
                            }
                            []
                    )
                |> Ui.toggle []
                    { flag = "showTokens"
                    , inHeader = False
                    , label = []
                    }
    in
    List.concatMap viewChapter outline
        ++ tokens


viewToc : Ui
viewToc =
    Ui.search [ Attr.placeholder "Search" ]
        { category = "search"
        , inHeader = False
        , label = []
        }
        searchToc
        |> Ui.inRegion Toc


body : Ui
body =
    viewWelcome ++ viewToc


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
                                    [ Attr.style "display" "inline-block"
                                    , Attr.style "position" "sticky"
                                    , Attr.style "top" "3rem"
                                    , Attr.style "vertical-align" "top"
                                    , Attr.style "margin" "0rem 3rem 0 0"
                                    ]

                        content =
                            Maybe.withDefault [] (renderedHtml.region Content)
                                |> Html.main_
                                    [ Attr.style "display" "inline-block"
                                    , Attr.style "max-width" "calc(100vw - 1em)"
                                    , Attr.style "overflow" "visible"
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

Source: [https://github.com/upsiflu/less-ui](https://github.com/upsiflu/less-ui)

Issues: [https://github.com/upsiflu/less-ui/issues](https://github.com/upsiflu/less-ui/issues)

🐌🐌🐌

_I'm always excited to hear your comments, questions, musings, issues or tips. Slip me a message via upsiflu{at}gmail.com or on the elm slack or in a github issue._

Have a beautiful day!

Flupsi



        
    """
