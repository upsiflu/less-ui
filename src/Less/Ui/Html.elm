module Less.Ui.Html exposing
    ( Html, html
    , toggle, goTo, bounce, filter, search
    , section, article, block, inline, disclose
    , ol, ul, keyedNode, nest
    , layout, arrangeOverDefaultRegions, Region(..)
    )

{-| Default types and functions for working with [elm/html](https://package.elm-lang.org/packages/elm/html/latest/) within [`Less.Ui`](Less-Ui)

@docs Html, html


# Create Links

[Read more in the `Link` module.](Less-Link)

@docs toggle, goTo, bounce, filter, search


# Wrap the DOM

@docs section, article, block, inline, disclose
@docs ol, ul, keyedNode, nest


# Provide a Layout

@docs layout, arrangeOverDefaultRegions, Region

-}

import Html
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed
import Html.Lazy
import Less.Link as Link exposing (Link, Mutation(..), State)
import Less.Ui as Ui
import List.Extra as List
import Maybe.Extra as Maybe


{-| 🐌
-}
type alias Html region narrowMsg msg =
    Ui.Ui
        region
        (HtmlList (Link.Msg msg))
        (Wrapper region narrowMsg msg)


{-| -}
type alias NarrowUi region narrowMsg =
    Html region narrowMsg narrowMsg


{-| 🐌
-}
html : HtmlList msg -> Html region_ narrowMsg_ msg
html =
    List.map (Html.map Link.AppMsg)
        >> Ui.singleton



---- Create links ----


{-| Toggle a `Flag` and show/hide the associated `Ui` accordingly.
Will add the flag when the link is opened in a new tab or shared, no matter its state in the current tab.

`Flag` is a string and may contain "=".

-}
toggle :
    List (Html.Attribute Never)
    ->
        { flag : Link.Flag
        , inHeader : Bool
        , label : HtmlList msg
        }
    -> Html region narrowMsg msg
    -> Html region narrowMsg msg
toggle attributes config =
    Toggle attributes config
        >> Ui.wrap


{-| Show a `Ui` according to what searchTerms are currently associated with a given category.

    -- Show a token for each SearchTerm in category "search=a"
    List.concatMap viewToken
        |> filter "search"

-}
filter :
    Link.Category
    -> (List Link.SearchTerm -> Html region narrowMsg msg)
    -> Html region narrowMsg msg
filter category =
    Filter category
        >> Ui.wrap


{-| Display a search box and show a `Ui` according to what searchTerms are currently associated with a given category.
-}
search :
    List (Html.Attribute Never)
    ->
        { category : Link.Category
        , inHeader : Bool
        , label : HtmlList msg
        }
    -> (List Link.SearchTerm -> Html region narrowMsg msg)
    -> Html region narrowMsg msg
search attributes config =
    Search config.category
        { attributes = attributes
        , inHeader = config.inHeader
        , label = config.label
        }
        >> Ui.wrap


{-| Navigate to a `destination` and show the nested `Ui` when it's reached.
-}
goTo :
    List (Html.Attribute Never)
    ->
        { destination : Link.Location
        , inHeader : Bool
        , label : HtmlList msg
        }
    -> Html region narrowMsg msg
    -> Html region narrowMsg msg
goTo attributes config =
    GoTo attributes config
        >> Ui.wrap


{-| Navigate to `there`, and from there, back `here`.
Will navigate `there` when the link is opened in a new tab or shared.
-}
bounce :
    List (Html.Attribute Never)
    ->
        { there : Link.Location
        , here : Link.Location
        , label : HtmlList msg
        }
    -> Html region narrowMsg msg
    -> Html region narrowMsg msg
bounce attributes config =
    Bounce attributes config
        >> Ui.wrap



---- Working with contingent transformations ----


{-| -}
type Wrapper region narrowMsg msg
    = Block { onlyInCurrentRegion : Bool } String (List (Html.Attribute msg)) (Html region narrowMsg msg)
    | Inline { onlyInCurrentRegion : Bool } String (List (Html.Attribute msg)) (Html region narrowMsg msg)
    | Ol (List (Html.Attribute msg)) (List ( String, Html region narrowMsg msg ))
    | Ul (List (Html.Attribute msg)) (List ( String, Html region narrowMsg msg ))
    | KeyedNode String (List (Html.Attribute msg)) (List ( String, Html region narrowMsg msg ))
    | Nested
        { regions : List region
        , combine :
            { makeInnerHtml :
                NarrowUi region narrowMsg
                -> Maybe (HtmlList (Link.Msg narrowMsg))
            }
            -> HtmlList (Link.Msg msg)
        }
    | Toggle
        (List (Html.Attribute Never))
        { flag : Link.Flag
        , inHeader : Bool
        , label : HtmlList msg
        }
        (Html region narrowMsg msg)
    | Filter Link.Category (List Link.SearchTerm -> Html region narrowMsg msg)
    | Search
        Link.Category
        { attributes : List (Html.Attribute Never)
        , inHeader : Bool
        , label : HtmlList msg
        }
        (List Link.SearchTerm -> Html region narrowMsg msg)
    | GoTo
        (List (Html.Attribute Never))
        { destination : Link.Location
        , inHeader : Bool
        , label : HtmlList msg
        }
        (Html region narrowMsg msg)
    | Bounce
        (List (Html.Attribute Never))
        { there : Link.Location
        , here : Link.Location
        , label : HtmlList msg
        }
        (Html region narrowMsg msg)


type alias HtmlList msg =
    List (Html.Html msg)


{-| From w3.org:

> The section element represents a generic section of a document or application. A section, in this context, is a thematic grouping of content, typically with a heading.

-}
section : List (Html.Attribute msg) -> Html region narrowMsg msg -> Html region narrowMsg msg
section =
    block "section"


{-| From w3.org:

> The article element represents a self-contained composition in a document, page, application, or site and that is, in principle, independently distributable or reusable, e.g. in syndication.

-}
article : List (Html.Attribute msg) -> Html region narrowMsg msg -> Html region narrowMsg msg
article =
    block "article"


{-| From w3.org:

> The details element represents a disclosure widget from which the user can obtain additional information or controls.

> The summary element child of the element, if any, represents the summary or legend of the details.

Notes:

  - You can auto-open it with `Attr.attribute "open" ""`
  - It will also auto-open if you search the page and a match is inside
  - It is impossible to animate
  - If you want animations and reproducibility, use `toggle` instead

-}
disclose : List (Html.Attribute msg) -> { summary : Html region narrowMsg msg, summaryAttrs : List (Html.Attribute msg) } -> Html region narrowMsg msg -> Html region narrowMsg msg
disclose attrs { summary, summaryAttrs } more =
    block "details" attrs (block "summary" summaryAttrs summary ++ more)


{-| Wrap all parts of the Ui that are in the current region in an arbitrary Html node.

    block "div" [] myUi

-}
block : String -> List (Html.Attribute msg) -> Html region narrowMsg msg -> Html region narrowMsg msg
block str attrs =
    Block { onlyInCurrentRegion = True } str attrs >> Ui.wrap


{-| Wrap all parts of the Ui that are in the current region in an arbitrary Html node.
Distinguishing between inline and block helps showing nice transitions.

    inline "span" [] myUi

-}
inline : String -> List (Html.Attribute msg) -> Html region narrowMsg msg -> Html region narrowMsg msg
inline str attrs =
    Inline { onlyInCurrentRegion = True } str attrs >> Ui.wrap


{-| Ordered List
-}
ol : List (Html.Attribute msg) -> List ( String, Html region narrowMsg msg ) -> Html region narrowMsg msg
ol attrs =
    Ol attrs >> Ui.wrap


{-| Unordered List
-}
ul : List (Html.Attribute msg) -> List ( String, Html region narrowMsg msg ) -> Html region narrowMsg msg
ul attrs =
    Ul attrs >> Ui.wrap


{-| Key a node with `Tuple.pair "uniqueKey"` such that the vDom diffing algorithm preserves it even when it changes position in the list.

Note [ul](#ul) and [ol](#ol).

The functionality is taken directly from the standard library `Html.Keyed`.

-}
keyedNode : String -> List (Html.Attribute msg) -> List ( String, Html region narrowMsg msg ) -> Html region narrowMsg msg
keyedNode tagName attrs =
    KeyedNode tagName attrs >> Ui.wrap


{-| Gives your Html widgets access to state information.

For example, if you want to extend a widget or form generator (`elm-any-type-forms`) that can only output Html
with `Ui` elements that alter and respond to the Url, then you need

  - a way to convert from `Ui (Msg narrowMsg)` to `Html (Msg narrowMsg)` nested inside the widget -> `makeInnerHtml`
  - a way to convert from the widget `Html (Msg msg)` to `Ui (Msg msg)` -> `combine`
  - a way to forward the current state to the nested `Ui`

Here is how you use this function:

1.  Write the `Ui` code for your widget extension.
    You can use all the local parameters your widget provides.

2.  Convert it to `Html (Msg narrowMsg)` using `makeInnerHtml`
    to make it fit inside your widget.

3.  Your widget will create `Html (Msg msg)`. The function you
    wrote for drawing the widget is the `combine` function that
    you can now `nest`.

**Caution:** If you use this functionality, the `Ui` will contain functions and will no longer support equality checks and serialisation.

**Note:** As of now, the type setup will limit you to **nest one layer deep**, i.e. the compiler will complain if you try to nest Ui in a widget that is part of the Ui nested in another widget.
This is because the inside of the widget uses a different message type than the outside of the widget. As of now, I see no other way to mitigate than to use code generation
in order to write out the `narrower` nested types. I've never done code generation, and I don't feel comfortable with long types, so I'll leave it to you.

-}
nest :
    { regions : List region
    , combine :
        { makeInnerHtml :
            NarrowUi region narrowMsg
            -> Maybe (HtmlList (Link.Msg narrowMsg))
        }
        -> HtmlList (Link.Msg msg)

    -- Plug it into your widget here
    }
    -> Html region narrowMsg msg
nest config =
    Ui.wrap (Nested config)



----- LAYOUT -----


{-| Note: If you want to use screen `region`s, replace the `arrange` field.
An example is given at [arrangeOverDefaultRegions](#arrangeOverDefaultRegions)

    import Less.Ui.Html exposing (layout)

    { layout | arrange = Less.Ui.Html.arrangeOverDefaultRegions }

-}
layout :
    Ui.Layout
        region
        (HtmlList (Link.Msg narrowMsg))
        (HtmlList (Link.Msg msg))
        (Wrapper region narrowMsg narrowMsg)
        (Wrapper region narrowMsg msg)
layout =
    { wrap = wrap
    , concat = concat
    , arrange = \{ header } -> Maybe.withDefault [] header
    }


{-| -}
wrap :
    { current : State, previous : Maybe State }
    -> Wrapper region narrowMsg msg
    ->
        Ui.Wrapper
            region
            (HtmlList (Link.Msg narrowMsg))
            (HtmlList (Link.Msg msg))
            (Wrapper region narrowMsg narrowMsg)
            (Wrapper region narrowMsg msg)
wrap states wrapper =
    let
        appAttr : List (Html.Attribute msg) -> List (Html.Attribute (Link.Msg msg))
        appAttr =
            List.map (Attr.map Link.AppMsg)

        appHtml : HtmlList msg -> HtmlList (Link.Msg msg)
        appHtml =
            List.map (Html.map Link.AppMsg)

        applyMutation : Mutation -> Html region narrowMsg msg -> Html region narrowMsg msg
        applyMutation mutation =
            let
                addAttributes :
                    { blockAttributes : List (Html.Attribute Never)
                    , inlineAttributes : List (Html.Attribute Never)
                    , vanishableAttributes : List (Html.Attribute Never)
                    }
                    -> Wrapper region narrowMsg msg
                    -> Wrapper region narrowMsg msg
                addAttributes { blockAttributes, inlineAttributes, vanishableAttributes } innerWrapper =
                    let
                        recurse : Html region narrowMsg msg -> Html region narrowMsg msg
                        recurse =
                            addAttributes { blockAttributes = blockAttributes, inlineAttributes = inlineAttributes, vanishableAttributes = vanishableAttributes }
                                |> Ui.mapWrapper

                        ( staticBlockAttributes, staticInlineAttributes ) =
                            ( List.map (Attr.map never) blockAttributes, List.map (Attr.map never) inlineAttributes )
                    in
                    case innerWrapper of
                        Block config tagName attrs contingent ->
                            Block config tagName (attrs ++ staticBlockAttributes) contingent

                        Inline config tagName attrs contingent ->
                            Inline config tagName (attrs ++ staticInlineAttributes) contingent

                        Ol attrs contingent ->
                            Ol (attrs ++ staticBlockAttributes) contingent

                        Ul attrs contingent ->
                            Ul (attrs ++ staticBlockAttributes) contingent

                        KeyedNode tagName attrs keyedElements ->
                            KeyedNode tagName (attrs ++ staticBlockAttributes) keyedElements

                        Nested { regions, combine } ->
                            Nested { regions = regions, combine = combine }

                        Toggle attrs { flag, inHeader, label } contingent ->
                            Toggle
                                (attrs ++ vanishableAttributes)
                                { flag = flag, inHeader = inHeader, label = label }
                                contingent

                        Filter category contingent ->
                            Filter
                                category
                                (contingent >> recurse)

                        Search category config contingent ->
                            Search
                                category
                                ((\a -> { a | attributes = a.attributes ++ vanishableAttributes }) config)
                                (contingent >> recurse)

                        GoTo attrs config contingent ->
                            GoTo (attrs ++ vanishableAttributes) config (recurse contingent)

                        Bounce attrs config contingent ->
                            Bounce (attrs ++ vanishableAttributes ++ [ Attr.class "HERE" ]) config (recurse contingent)

                appearing : { block : List (Html.Attribute Never), inline : List (Html.Attribute Never) }
                appearing =
                    { block =
                        visible.block
                            ++ [ Attr.style "transition" "max-height .2s, margin .2s, padding .2s, opacity .2s" ]
                    , inline =
                        visible.inline
                            ++ [ Attr.style "transition" "font-size .2s 0s, opacity .2s" ]
                    }

                visible : { block : List (Html.Attribute Never), inline : List (Html.Attribute Never) }
                visible =
                    { block =
                        [ Attr.style "max-height" "100vh" ]
                    , inline =
                        []
                    }

                disappearing : { block : List (Html.Attribute Never), inline : List (Html.Attribute Never) }
                disappearing =
                    { block =
                        hidden.block
                            ++ [ Attr.style "transition" "max-height .2s, margin .2s, padding .2s, opacity .2s" ]
                    , inline =
                        hidden.inline
                            ++ [ Attr.style "transition" "font-size .2s 0s, opacity .2s .0s" ]
                    }

                hidden : { block : List (Html.Attribute Never), inline : List (Html.Attribute Never) }
                hidden =
                    { block =
                        [ Attr.attribute "aria-hidden" "true"
                        , Attr.tabindex -1
                        , Attr.style "opacity" "0"
                        , Attr.style "pointer-events" "none;"
                        , Attr.style "max-height" "0"
                        , Attr.style "overflow" "hidden"
                        , Attr.style "margin" "0"
                        , Attr.style "padding" "0"
                        , Attr.style "border-width" "0"
                        ]
                    , inline =
                        [ Attr.attribute "aria-hidden" "true"
                        , Attr.tabindex -1
                        , Attr.style "opacity" "0"
                        , Attr.style "pointer-events" "none;"
                        , Attr.style "font-size" "0"
                        ]
                    }

                outOfFlow : List (Html.Attribute Never)
                outOfFlow =
                    [ Attr.style "display" "none" ]
            in
            Ui.mapWrapper
                (addAttributes <|
                    case mutation of
                        StateEntered _ ->
                            { blockAttributes = Attr.class "state-entered" :: appearing.block
                            , inlineAttributes = Attr.class "state-entered" :: appearing.inline
                            , vanishableAttributes = Attr.class "state-entered" :: appearing.inline
                            }

                        StateInside _ ->
                            { blockAttributes = [ Attr.class "state-inside" ]
                            , inlineAttributes = [ Attr.class "state-inside" ]
                            , vanishableAttributes = [ Attr.class "state-inside" ]
                            }

                        StateLeft _ ->
                            { blockAttributes = Attr.class "state-left" :: disappearing.block
                            , inlineAttributes = Attr.class "state-left" :: disappearing.inline
                            , vanishableAttributes = Attr.class "state-left" :: disappearing.inline ++ outOfFlow
                            }

                        StateOutside _ ->
                            { blockAttributes = Attr.class "state-outside" :: hidden.block
                            , inlineAttributes = Attr.class "state-outside" :: hidden.inline
                            , vanishableAttributes = Attr.class "state-outside" :: hidden.inline ++ outOfFlow
                            }

                        SwitchedOn ->
                            { blockAttributes = Attr.class "switched-on" :: appearing.block
                            , inlineAttributes = Attr.class "switched-on" :: appearing.inline
                            , vanishableAttributes = Attr.class "switched-on" :: appearing.inline
                            }

                        StillOn ->
                            { blockAttributes = [ Attr.class "still-on" ]
                            , inlineAttributes = [ Attr.class "still-on" ]
                            , vanishableAttributes = [ Attr.class "still-on" ]
                            }

                        SwitchedOff ->
                            { blockAttributes = Attr.class "switched-off" :: disappearing.block
                            , inlineAttributes = Attr.class "switched-off" :: disappearing.inline
                            , vanishableAttributes = Attr.class "switched-off" :: disappearing.inline ++ outOfFlow
                            }

                        StillOff ->
                            { blockAttributes = Attr.class "still-off" :: hidden.block
                            , inlineAttributes = Attr.class "still-off" :: hidden.inline
                            , vanishableAttributes = Attr.class "still-off" :: hidden.inline ++ outOfFlow
                            }
                )

        getMutation : Link -> Maybe String -> Mutation
        getMutation =
            Link.mutationFromTwoStates states

        {- map the nested wrappers to appear/disappear -}
        labelAttributesByMutation : Mutation -> List (Html.Attribute msg)
        labelAttributesByMutation mutation =
            case mutation of
                StateEntered currentSet ->
                    [ Attr.attribute "aria-current" currentSet
                    , Attr.attribute "data-set" currentSet
                    , Attr.class "state-entered"
                    ]

                StateInside currentSet ->
                    [ Attr.attribute "aria-current" currentSet
                    , Attr.attribute "data-set" currentSet
                    , Attr.class "state-inside"
                    ]

                StateLeft currentSet ->
                    [ Attr.attribute "aria-current" "false"
                    , Attr.attribute "data-set" currentSet
                    , Attr.class "state-left"
                    ]

                StateOutside currentSet ->
                    [ Attr.attribute "aria-current" "false"
                    , Attr.attribute "data-set" currentSet
                    , Attr.class "state-outside"
                    ]

                SwitchedOn ->
                    [ Attr.attribute "role" "switch"
                    , Attr.attribute "aria-checked" "true"
                    , Attr.class "state-entered"
                    ]

                StillOn ->
                    [ Attr.attribute "role" "switch"
                    , Attr.attribute "aria-checked" "true"
                    , Attr.class "state-inside"
                    ]

                SwitchedOff ->
                    [ Attr.attribute "role" "switch"
                    , Attr.attribute "aria-checked" "false"
                    , Attr.class "state-left"
                    ]

                StillOff ->
                    [ Attr.attribute "role" "switch"
                    , Attr.attribute "aria-checked" "false"
                    , Attr.class "state-outside"
                    ]

        wrapKeyedElements : (List ( String, b ) -> a) -> List ( String, List b ) -> List a
        wrapKeyedElements fu =
            List.concatMap
                (\( key, items ) ->
                    List.indexedMap (\i a -> ( key ++ "." ++ String.fromInt i, a )) items
                )
                >> fu
                >> List.singleton

        wrapListElements : (List ( a, Html.Html (Link.Msg msg) ) -> b) -> List ( a, HtmlList (Link.Msg msg) ) -> List b
        wrapListElements fu =
            List.map
                (\( key, items ) ->
                    ( key, Html.li [] items )
                )
                >> fu
                >> List.singleton
    in
    case wrapper of
        Block { onlyInCurrentRegion } str attrs elements ->
            let
                howToWrap : HtmlList (Link.Msg msg) -> HtmlList (Link.Msg msg)
                howToWrap =
                    Html.node str (appAttr attrs) >> List.singleton
            in
            Ui.Wrapped
                { howToWrapCurrentRegion = howToWrap
                , howToWrapOtherRegions =
                    if onlyInCurrentRegion then
                        identity

                    else
                        howToWrap
                }
                elements

        Inline { onlyInCurrentRegion } str attrs elements ->
            let
                howToWrap : HtmlList (Link.Msg msg) -> HtmlList (Link.Msg msg)
                howToWrap =
                    Html.node str (appAttr attrs) >> List.singleton
            in
            Ui.Wrapped
                { howToWrapCurrentRegion = howToWrap
                , howToWrapOtherRegions =
                    if onlyInCurrentRegion then
                        identity

                    else
                        howToWrap
                }
                elements

        Ol attrs keyedElements ->
            Ui.Keyed
                { howToWrap =
                    Html.Keyed.ol (appAttr attrs)
                        |> wrapListElements
                }
                keyedElements

        Ul attrs keyedElements ->
            Ui.Keyed
                { howToWrap =
                    Html.Keyed.ul (appAttr attrs)
                        |> wrapListElements
                }
                keyedElements

        KeyedNode tagName attrs keyedElements ->
            Ui.Keyed
                { howToWrap =
                    Html.Keyed.node tagName (appAttr attrs)
                        |> wrapKeyedElements
                }
                keyedElements

        Nested { regions, combine } ->
            Ui.Nested
                { regions = regions
                , narrowLayout = Ui.applyStates states layout
                , combine = combine
                }

        Toggle attributes { flag, inHeader, label } contingent ->
            let
                link : Link
                link =
                    Link.Toggle flag

                mutation : Mutation
                mutation =
                    getMutation link Nothing
            in
            applyMutation mutation contingent
                |> Ui.Labeled
                    { label =
                        appHtml
                            [ Html.a
                                (Attr.href (Link.toHref link)
                                    :: labelAttributesByMutation mutation
                                    ++ List.map (Attr.map never) attributes
                                )
                                label
                            ]
                    , inHeader = inHeader
                    }

        Filter category contingent ->
            let
                inHeader : Bool
                inHeader =
                    True

                mutation : Mutation
                mutation =
                    getMutation (link "") Nothing

                link : Link.SearchTerm -> Link
                link newSearchTerm =
                    Link.Filter { category = category, searchTerm = newSearchTerm }
            in
            applyMutation mutation (contingent (List.unique (Link.getSpaceSeparatedSearchTerms category states.current)))
                |> Ui.Labeled
                    { label = []
                    , inHeader = inHeader
                    }

        Search category config contingent ->
            let
                spaceSeparatedSearchTerms : List Link.SearchTerm
                spaceSeparatedSearchTerms =
                    Link.getSpaceSeparatedSearchTerms category states.current

                viewLabel : HtmlList (Link.Msg msg)
                viewLabel =
                    appHtml config.label
                        ++ [ Html.input
                                (Attr.value (String.join " " spaceSeparatedSearchTerms)
                                    :: Events.onInput
                                        (\newSearchTerm ->
                                            Link.UrlCmd (link newSearchTerm)
                                        )
                                    :: appAttr (Attr.title category :: labelAttributesByMutation mutation)
                                    ++ List.map (Attr.map never) config.attributes
                                )
                                []
                           ]

                mutation : Mutation
                mutation =
                    getMutation (link "") Nothing

                link : Link.SearchTerm -> Link
                link newSearchTerm =
                    Link.Filter { category = category, searchTerm = newSearchTerm }
            in
            contingent (List.unique spaceSeparatedSearchTerms)
                |> Ui.Labeled
                    { label = viewLabel
                    , inHeader = config.inHeader
                    }

        GoTo attributes { destination, inHeader, label } contingent ->
            let
                link : Link
                link =
                    Link.GoTo (Link.parseLocation destination)

                mutation : Mutation
                mutation =
                    getMutation link (Just "page")
            in
            applyMutation mutation contingent
                |> Ui.Labeled
                    { label =
                        appHtml
                            [ Html.a
                                (Attr.href (Link.toHref link)
                                    :: labelAttributesByMutation mutation
                                    ++ List.map (Attr.map never) attributes
                                )
                                label
                            ]
                    , inHeader = inHeader
                    }

        Bounce attributes { there, here, label } contingent ->
            let
                link : Link
                link =
                    Link.Bounce { there = Link.parseLocation there, here = Link.parseLocation here }

                mutation : Mutation
                mutation =
                    getMutation link (Just "page")
            in
            applyMutation mutation contingent
                |> Ui.Labeled
                    { label =
                        appHtml
                            [ Html.a
                                (Attr.href (Link.toHref link)
                                    :: labelAttributesByMutation mutation
                                    ++ List.map (Attr.map never) attributes
                                )
                                label
                            ]
                    , inHeader = True
                    }


{-| -}
concat : List (List a) -> List a
concat =
    List.concat



---- Default Regions ----


{-| **Scene:** the Item's editable contents and overlays

**Control:** Toolbar, Property sheet

**Info:** Status bar, Help screen, Tooltip bubble, Snack bar

-}
type Region
    = Scene
    | Info
    | Control


{-|

    Lays out a traditional application layout with a sticky `Header`,
    scrolling `Scene`, and both `Control` and `Info` fixed to the bottom.

-}
arrangeOverDefaultRegions : { header : Maybe (HtmlList msg), region : Region -> Maybe (HtmlList msg) } -> HtmlList msg
arrangeOverDefaultRegions rendered =
    Maybe.values
        [ Maybe.map
            (Html.Lazy.lazy2
                Html.header
                [ Attr.class "header", Attr.style "position" "sticky" ]
            )
            rendered.header
        , Maybe.map
            (Html.Lazy.lazy2
                Html.main_
                [ Attr.class "scene" ]
            )
            (rendered.region Scene)
        , Maybe.map
            (Html.Lazy.lazy2
                Html.div
                [ Attr.class "info", Attr.style "position" "fixed", Attr.style "bottom" "0", Attr.style "right" "0" ]
            )
            (rendered.region Info)
        , Maybe.map
            (Html.Lazy.lazy2
                Html.div
                [ Attr.class "control", Attr.style "position" "fixed", Attr.style "bottom" "0" ]
            )
            (rendered.region Control)
        ]
