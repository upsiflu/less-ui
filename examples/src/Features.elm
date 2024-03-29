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

# Less-Ui Demo

How can we make our user interface code more cohesive? 
Less pushing around `views` across the DOM, less Ui state in the model.

This demo implements some of the patterns I have found useful in my work. 
It is a self-explaining walkthrough and presents its own code.

To run it yourself, `git clone https://github.com/upsiflu/less-ui/` into your computer,
then follow the instructions [in the Readme](https://github.com/upsiflu/less-ui/).

Or open [the Elm module](https://github.com/upsiflu/less-ui/blob/main/examples/src/Features.elm) and follow along!

To use the patterns in your own frontends, install Elm and `elm install upsiflu/less-ui`.

Btw, here is the code that created this message:

```elm
viewWelcome : Ui
viewWelcome =
    md  ∞
        |> Ui.inRegion Content
        |> Ui.goTo []
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
            , label = [ Html.button [] [ Html.text "⌂" ] ]
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
import Less.Ui as Ui
import Less.Ui.Html as Ui exposing (layout)
import Markdown exposing (md)
```

```elm
type alias Ui =
    Ui.Html 
        Region 
        () 
        ()
```

An Html Ui has three type parameters:

1. The [Screen Region](Screen+Regions) of a snipped sets where on the screen it will appear.
For example, `#!elm ui |> inRegion X` moves the Ui to region "X". 
More about Regions [in the next chapter](Screen+Regions).
1. The second parameter is the message type. This app is completely Url-driven and has
neither a model nor any messages, so the message type is "unit" `#!elm ()`.
Any message that patterns such as "search" require are managed internally by [Less.Application](Applications+without+Ui+state).
1. The third parameter is the message type of nested Html. It's only relevant if you want
to nest Ui code inside a function that transforms from Html a to Html b. For example with
`elm-any-type-forms`. 
"""


type alias Ui =
    Ui.Html Region () ()


regionExplanation : String
regionExplanation =
    """
# Screen Regions

Define your regions, then assign your Ui snippets their place on the screen
with `#!elm Ui.inRegion`. This is how this text appears in the `#!elm Content` region 
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
    viewWelcome ++ viewToc ++ Markdown.syntaxHighlight ++ Ui.animations
```

>> _To compose the body of the app, we append two `#!elm Ui`s. 
  This is possible because a `#!elm Ui` is a `#!elm List`.
  As you may have guessed, the last two are `<style>` tags._
               

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
    Ui.search [ Attr.placeholder "Search" ]
        { category = "search"
        , inHeader = False
        , label = []
        }
        searchOutline
        |> Ui.inRegion Toc
```


>> _The `#!elm search` pattern produces an input field where users can enter space-separated search terms.
In `#!elm searchToc`, we respond dynamically to these changes._

```elm
searchOutline : List String -> Ui
searchOutline searchTerms =
    let
```
>> _For each chapter, render links to its heading(s). If the chapter doesn't contain all search terms, 
   hide the corresponding links with a graceful css transition._
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
Ui.toggle []
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

[Set some search terms (if you haven't already)](?filter=search=pattern+Url)

[Toggle me the tokens](?toggle=showTokens)

The markdown for these Links are 
`#!md [Set some search terms (if you haven't already)](?filter=search=pattern+Url)` and
`#!md [Toggle me the tokens](?toggle=showTokens)`
,
and here is the `#!elm toggle` pattern that hides and shows the toggles under the TOC:

```elm
Ui.toggle []
    { flag = "showTokens"
    , inHeader = False
    , label = []
    }
    ...
```

# Layout

We are extending `#!elm Ui.layout`,
using a custom `#!elm arrange` function that receives the rendered Html snippets per screen region.

```elm
view : () -> Less.Document ()
view () =
    { body = body
    , layout =
        { layout 
        | arrange =
            \\renderedHtml ->
                [ renderedHtml.header
                    |> Html.header [ ... ]

                , renderedHtml.region Toc
                    |> Html.nav [ ... ]

                , renderedHtml.region Content
                    |> Html.main_ [ ... ]
                ]
        }
    , title = "Less-Ui Walkthrough"
    }
        |> Less.mapDocument identity
```


    """


body : Ui
body =
    viewWelcome
        ++ viewToc
        ++ testTheToggle
        ++ Markdown.syntaxHighlight
        ++ Ui.animations
        -- Let's make the default `elm make` Html a bit more delightful...
        ++ Ui.html
            [ Html.node "meta"
                [ Attr.name "viewport", Attr.attribute "content" "width=device-width, initial-scale=1" ]
                []
            , Html.node "style"
                []
                [ Html.text "body {padding: .5em;}" ]
            ]


outline : List String
outline =
    [ uiExplanation
    , regionExplanation
    , viewExplanation
    , mainExplanation
    , whereToGoNext
    ]


viewToc : Ui
viewToc =
    Ui.search [ Attr.placeholder "Search" ]
        { category = "search"
        , inHeader = False
        , label = []
        }
        searchOutline
        |> Ui.inRegion Toc


testTheToggle : Ui
testTheToggle =
    Ui.toggle []
        { flag = "properties", inHeader = False, label = [ Html.text "Property Sheet" ] }
        (Ui.singleton [ Html.text "PROP\u{00A0}SHEET ON" ])
        |> Ui.inRegion Toc


searchOutline : List String -> Ui
searchOutline searchTerms =
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


view : () -> Less.Document ()
view () =
    { body = body
    , layout =
        { layout
            | arrange =
                \renderedHtml ->
                    [ renderedHtml.header
                        |> Html.header
                            [ Attr.class "header"
                            , Attr.style "position" "sticky"
                            , Attr.style "top" "0.5em"
                            ]
                    , renderedHtml.region Toc
                        |> Html.nav
                            [ Attr.style "display" "inline-block"
                            , Attr.style "position" "sticky"
                            , Attr.style "top" "2rem"
                            , Attr.style "vertical-align" "top"
                            , Attr.style "margin" "2rem 3rem 0 0"
                            ]
                    , renderedHtml.region Content
                        |> Html.main_
                            [ Attr.style "display" "inline-block"
                            , Attr.style "max-width" "calc(100vw - 1em)"
                            , Attr.style "width" "min-content"
                            , Attr.style "overflow" "visible"
                            ]
                    ]
        }
    , title = "Less-Ui Walkthrough"
    }
        |> Less.mapDocument identity


mainExplanation : String
mainExplanation =
    """# Applications without Ui state


A `#!elm Less.Application` hides the Ui states in the Url. In your Ui, you can then use
prefabricated patterns such as `#!elm Ui.toggle`, `#!elm Ui.goTo` or `#!elm Ui.filter` to
create progressive disclosures, routed pages, or query assignments.

```elm
main : Less.Application () ()
main =
    Less.application
        { init = ( (), Cmd.none )
        , update = \\() () -> ( (), Cmd.none )
        , view = view
        }
```

>> _As you see, both our `#!elm model` and `#!elm msg` type are `#!elm ()`, meaning the app
doesn't manage any state of its wn. Instead, `#!elm Less` manages Url transition opaquely._"""


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

📖 Docs: [https://package.elm-lang.org/packages/upsiflu/less-ui/latest](https://package.elm-lang.org/packages/upsiflu/less-ui/latest)

🐌 Source: [https://github.com/upsiflu/less-ui](https://github.com/upsiflu/less-ui)

💭 Issues: [https://github.com/upsiflu/less-ui/issues](https://github.com/upsiflu/less-ui/issues)



_I'm always excited to hear your comments, questions, musings, issues or tips. Slip me a message via upsiflu{at}gmail.com or on the elm slack or in a github issue._

Have a beautiful day!

Flupsi



        
    """
