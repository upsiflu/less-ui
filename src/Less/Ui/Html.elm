module Less.Ui.Html exposing
    ( Ui, singleton
    , toggle, goTo, bounce, filter, search
    , section, article, block, inline
    , ol, ul, keyedNode, nest
    , layout, arrangeOverDefaultRegions, Region(..)
    )

{-| Default types and functions for working with [elm/html](https://package.elm-lang.org/packages/elm/html/latest/) within [`Less.Ui`](Less-Ui)

@docs Ui, singleton


# Create Links

[Read more in the `Link` module.](Less-Link)

@docs toggle, goTo, bounce, filter, search


# Wrap the DOM

@docs section, article, block, inline
@docs ol, ul, keyedNode, nest


# Provide a Layout

@docs layout, arrangeOverDefaultRegions, Region

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed
import Html.Lazy
import Less.Link as Link exposing (Link, Mutation(..), State)
import Less.Ui as Ui
import Maybe.Extra as Maybe


{-| 🐌
-}
type alias Ui region narrowMsg msg =
    Ui.Ui
        region
        (HtmlList (Link.Msg msg))
        (Wrapper region narrowMsg msg)


{-| -}
type alias NarrowUi region narrowMsg =
    Ui region narrowMsg narrowMsg


{-| 🐌
-}
singleton : List (Html msg) -> Ui region_ narrowMsg_ msg
singleton =
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
        , isInline : Bool
        , label : HtmlList msg
        }
    -> Ui region narrowMsg msg
    -> Ui region narrowMsg msg
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
    -> (List Link.SearchTerm -> Ui region narrowMsg msg)
    -> Ui region narrowMsg msg
filter category =
    Filter category Nothing
        >> Ui.wrap


{-| Display a search box and show a `Ui` according to what searchTerms are currently associated with a given category.

    singleton [ Html.h1 [] [ Html.text "Two Search Boxes" ] ]
        ++ search []
            { category = "search=a"
            , isInline = True
            , label = []
            }
            (\_ -> [])
        ++ search []
            { category = "search=b"
            , isInline = True
            , label = []
            }
            (\_ -> [])
        ++ filter "search"
            -- will receive a subset of ["a=...", "b=..."] as SearchTerms
            (List.concatMap resultsPerSearchTerm
                >> List.concatMap viewSeachResult
            )

-}
search :
    List (Html.Attribute Never)
    ->
        { category : Link.Category
        , isInline : Bool
        , label : HtmlList msg
        }
    -> (List Link.SearchTerm -> Ui region narrowMsg msg)
    -> Ui region narrowMsg msg
search attributes config =
    Filter config.category
        (Just
            { attributes = attributes
            , isInline = config.isInline
            , label = config.label
            }
        )
        >> Ui.wrap


{-| Navigate to a `destination` and show the nested `Ui` when it's reached.
-}
goTo :
    List (Html.Attribute Never)
    ->
        { destination : Link.Location
        , isInline : Bool
        , label : HtmlList msg
        }
    -> Ui region narrowMsg msg
    -> Ui region narrowMsg msg
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
    -> Ui region narrowMsg msg
    -> Ui region narrowMsg msg
bounce attributes config =
    Bounce attributes config
        >> Ui.wrap



---- Working with contingent transformations ----


{-| -}
type Wrapper region narrowMsg msg
    = Block { onlyInCurrentRegion : Bool } String (List (Html.Attribute msg)) (Ui region narrowMsg msg)
    | Inline { onlyInCurrentRegion : Bool } String (List (Html.Attribute msg)) (Ui region narrowMsg msg)
    | Ol (List (Html.Attribute msg)) (List ( String, Ui region narrowMsg msg ))
    | Ul (List (Html.Attribute msg)) (List ( String, Ui region narrowMsg msg ))
    | KeyedNode String (List (Html.Attribute msg)) (List ( String, Ui region narrowMsg msg ))
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
        , isInline : Bool
        , label : HtmlList msg
        }
        (Ui region narrowMsg msg)
    | Filter
        Link.Category
        (Maybe
            { attributes : List (Html.Attribute Never)
            , isInline : Bool
            , label : HtmlList msg
            }
        )
        (List Link.SearchTerm -> Ui region narrowMsg msg)
    | GoTo
        (List (Html.Attribute Never))
        { destination : Link.Location
        , isInline : Bool
        , label : HtmlList msg
        }
        (Ui region narrowMsg msg)
    | Bounce
        (List (Html.Attribute Never))
        { there : Link.Location
        , here : Link.Location
        , label : HtmlList msg
        }
        (Ui region narrowMsg msg)


type alias HtmlList msg =
    List (Html msg)


{-| From w3.org:

> The section element represents a generic section of a document or application. A section, in this context, is a thematic grouping of content, typically with a heading.

-}
section : List (Html.Attribute msg) -> Ui region narrowMsg msg -> Ui region narrowMsg msg
section =
    block "section"


{-| From w3.org:

> The article element represents a self-contained composition in a document, page, application, or site and that is, in principle, independently distributable or reusable, e.g. in syndication.

-}
article : List (Html.Attribute msg) -> Ui region narrowMsg msg -> Ui region narrowMsg msg
article =
    block "article"


{-| Wrap all parts of the Ui that are in the current region in an arbitrary Html node.

    block "div" [] myUi

-}
block : String -> List (Html.Attribute msg) -> Ui region narrowMsg msg -> Ui region narrowMsg msg
block str attrs =
    Block { onlyInCurrentRegion = True } str attrs >> Ui.wrap


{-| Wrap all parts of the Ui that are in the current region in an arbitrary Html node.
Distinguishing between inline and block helps showing nice transitions.

    inline "span" [] myUi

-}
inline : String -> List (Html.Attribute msg) -> Ui region narrowMsg msg -> Ui region narrowMsg msg
inline str attrs =
    Inline { onlyInCurrentRegion = True } str attrs >> Ui.wrap


{-| Ordered List
-}
ol : List (Html.Attribute msg) -> List ( String, Ui region narrowMsg msg ) -> Ui region narrowMsg msg
ol attrs =
    Ol attrs >> Ui.wrap


{-| Unordered List
-}
ul : List (Html.Attribute msg) -> List ( String, Ui region narrowMsg msg ) -> Ui region narrowMsg msg
ul attrs =
    Ul attrs >> Ui.wrap


{-| Key a node with `Tuple.pair "uniqueKey"` such that the vDom diffing algorithm preserves it even when it changes position in the list.

Note [ul](#ul) and [ol](#ol).

The functionality is taken directly from the standard library `Html.Keyed`.

-}
keyedNode : String -> List (Html.Attribute msg) -> List ( String, Ui region narrowMsg msg ) -> Ui region narrowMsg msg
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
    -> Ui region narrowMsg msg
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


addAttributes :
    { blockAttributes : List (Html.Attribute Never)
    , inlineAttributes : List (Html.Attribute Never)
    }
    -> Wrapper region narrowMsg msg
    -> Wrapper region narrowMsg msg
addAttributes { blockAttributes, inlineAttributes } wrapper =
    let
        recurse : Ui region narrowMsg msg -> Ui region narrowMsg msg
        recurse =
            addAttributes { blockAttributes = blockAttributes, inlineAttributes = inlineAttributes }
                |> Ui.mapWrapper

        ( staticBlockAttributes, staticInlineAttributes ) =
            ( List.map (Attr.map never) blockAttributes, List.map (Attr.map never) inlineAttributes )
    in
    case wrapper of
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

        Toggle attrs { flag, isInline, label } contingent ->
            Toggle (attrs ++ inlineAttributes) { flag = flag, isInline = isInline, label = label } contingent

        Filter category maybeConfig contingent ->
            Filter category maybeConfig (contingent >> recurse)

        GoTo attrs config contingent ->
            GoTo (attrs ++ inlineAttributes) config (recurse contingent)

        Bounce attrs config contingent ->
            Bounce (attrs ++ inlineAttributes) config (recurse contingent)


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

        applyMutation : Mutation -> Ui region narrowMsg msg -> Ui region narrowMsg msg
        applyMutation mutation =
            let
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
                        [ Attr.style "max-height" "100vh"
                        , Attr.style "overflow" "hidden"
                        ]
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
            in
            Ui.mapWrapper
                (addAttributes <|
                    case mutation of
                        StateEntered _ ->
                            { blockAttributes = Attr.class "state-entered" :: appearing.block
                            , inlineAttributes = Attr.class "state-entered" :: appearing.inline
                            }

                        StateInside _ ->
                            { blockAttributes = [ Attr.class "state-inside" ]
                            , inlineAttributes = [ Attr.class "state-inside" ]
                            }

                        StateLeft _ ->
                            { blockAttributes = Attr.class "state-left" :: disappearing.block
                            , inlineAttributes = Attr.class "state-left" :: disappearing.inline
                            }

                        StateOutside _ ->
                            { blockAttributes = Attr.class "state-outside" :: hidden.block
                            , inlineAttributes = Attr.class "state-outside" :: hidden.inline
                            }

                        SwitchedOn ->
                            { blockAttributes = Attr.class "switched-on" :: appearing.block
                            , inlineAttributes = Attr.class "switched-on" :: appearing.inline
                            }

                        StillOn ->
                            { blockAttributes = [ Attr.class "still-on" ]
                            , inlineAttributes = [ Attr.class "still-on" ]
                            }

                        SwitchedOff ->
                            { blockAttributes = Attr.class "switched-off" :: disappearing.block
                            , inlineAttributes = Attr.class "switched-off" :: disappearing.inline
                            }

                        StillOff ->
                            { blockAttributes = Attr.class "still-off" :: hidden.block
                            , inlineAttributes = Attr.class "still-off" :: hidden.inline
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
    in
    case wrapper of
        Block { onlyInCurrentRegion } str attrs elements ->
            let
                howToWrap : List (Html (Link.Msg msg)) -> List (Html (Link.Msg msg))
                howToWrap =
                    Html.node str (appAttr attrs) >> List.singleton
            in
            Ui.Wrapped
                { howToWrapCurrentRegion = howToWrap
                , howToWrapOtherRegions =
                    if onlyInCurrentRegion then
                        howToWrap

                    else
                        identity
                }
                elements

        Inline { onlyInCurrentRegion } str attrs elements ->
            let
                howToWrap : List (Html (Link.Msg msg)) -> List (Html (Link.Msg msg))
                howToWrap =
                    Html.node str (appAttr attrs) >> List.singleton
            in
            Ui.Wrapped
                { howToWrapCurrentRegion = howToWrap
                , howToWrapOtherRegions =
                    if onlyInCurrentRegion then
                        howToWrap

                    else
                        identity
                }
                elements

        Ol attrs keyedElements ->
            Ui.Keyed
                { howToWrap =
                    Html.Keyed.ol (appAttr attrs)
                        |> wrapKeyedElements
                }
                keyedElements

        Ul attrs keyedElements ->
            Ui.Keyed
                { howToWrap =
                    Html.Keyed.ul (appAttr attrs)
                        |> wrapKeyedElements
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

        Toggle attributes { flag, isInline, label } contingent ->
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
                    , isInline = isInline
                    }

        Filter category maybeConfig contingent ->
            let
                isInline : Bool
                isInline =
                    Maybe.map .isInline maybeConfig
                        |> Maybe.withDefault True

                link : Link.SearchTerm -> Link
                link newSearchTerm =
                    Link.Filter { category = category, searchTerm = newSearchTerm }

                mutation : Mutation
                mutation =
                    getMutation (link "") Nothing

                searchTerms : List Link.SearchTerm
                searchTerms =
                    Link.getStateSearchTerms category states.current
            in
            applyMutation mutation (contingent searchTerms)
                |> Ui.Labeled
                    { label =
                        case maybeConfig of
                            Just { attributes, label } ->
                                appHtml label
                                    ++ [ Html.input
                                            (Attr.value (String.join " " searchTerms)
                                                :: Events.onInput
                                                    (\newSearchTerm ->
                                                        Link.UrlCmd (link newSearchTerm)
                                                    )
                                                :: appAttr (Attr.title category :: labelAttributesByMutation mutation)
                                                ++ List.map (Attr.map never) attributes
                                            )
                                            []
                                       ]

                            Nothing ->
                                []
                    , isInline = isInline
                    }

        GoTo attributes { destination, isInline, label } contingent ->
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
                    , isInline = isInline
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
                    , isInline = True
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
arrangeOverDefaultRegions : { header : Maybe (List (Html msg)), region : Region -> Maybe (List (Html msg)) } -> List (Html msg)
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
