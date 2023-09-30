module Less.Ui.Html exposing
    ( Html, html
    , toggle, goTo, bounce, filter, search
    , section, article, node, disclose
    , ol, ul, keyedNode, nest
    , layout, animations, arrangeOverDefaultRegions, Region(..)
    )

{-| Default types and functions for working with [elm/html](https://package.elm-lang.org/packages/elm/html/latest/) within [`Less.Ui`](Less-Ui)

@docs Html, html


# Create Links

[Read more in the `Link` module.](Less-Link)

@docs toggle, goTo, bounce, filter, search


# Wrap the DOM

@docs section, article, node, disclose
@docs ol, ul, keyedNode, nest


# Provide a Layout

@docs layout, animations, arrangeOverDefaultRegions, Region

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
    = Node { onlyInCurrentRegion : Bool } String (List (Html.Attribute msg)) (Html region narrowMsg msg)
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
    node "section"


{-| From w3.org:

> The article element represents a self-contained composition in a document, page, application, or site and that is, in principle, independently distributable or reusable, e.g. in syndication.

-}
article : List (Html.Attribute msg) -> Html region narrowMsg msg -> Html region narrowMsg msg
article =
    node "article"


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
    node "details" attrs (node "summary" summaryAttrs summary ++ more)


{-| Wrap all parts of the Ui that are in the current region in an arbitrary Html node.

    node "div" [] myUi

-}
node : String -> List (Html.Attribute msg) -> Html region narrowMsg msg -> Html region narrowMsg msg
node str attrs =
    Node { onlyInCurrentRegion = True } str attrs >> Ui.wrap


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
    , arrange = .header
    }


getAttributes :
    Mutation
    ->
        { childAttributes : List ( String, String )
        , childSelector : ( String, String )
        , labelAttributes : List ( String, String )
        , labelSelector : ( String, String )
        }
getAttributes mutation =
    case mutation of
        StateEntered currentSet ->
            { childAttributes = []
            , childSelector = ( "data-mutation", "state-entered" )
            , labelAttributes =
                [ ( "aria-current", currentSet )
                , ( "data-set", currentSet )
                ]
            , labelSelector = ( "data-mutation", "label-state-entered" )
            }

        StateInside currentSet ->
            { childAttributes = []
            , childSelector =
                ( "data-mutation", "state-inside" )
            , labelAttributes =
                [ ( "aria-current", currentSet )
                , ( "data-set", currentSet )
                ]
            , labelSelector = ( "data-mutation", "label-state-inside" )
            }

        StateLeft currentSet ->
            { childAttributes =
                [ ( "aria-hidden", "true" ) ]
            , childSelector = ( "data-mutation", "state-left" )
            , labelAttributes =
                [ ( "aria-current", "false" )
                , ( "data-set", currentSet )
                ]
            , labelSelector = ( "data-mutation", "label-state-left" )
            }

        StateOutside currentSet ->
            { childAttributes = []
            , childSelector =
                ( "data-mutation", "state-outside" )
            , labelAttributes =
                [ ( "aria-current", "false" )
                , ( "data-set", currentSet )
                ]
            , labelSelector = ( "data-mutation", "label-state-outside" )
            }

        SwitchedOn ->
            { childAttributes = []
            , childSelector =
                ( "data-mutation", "switched-on" )
            , labelAttributes =
                [ ( "role", "switch" )
                , ( "aria-checked", "true" )
                ]
            , labelSelector = ( "data-mutation", "label-switched-on" )
            }

        StillOn ->
            { childAttributes = []
            , childSelector =
                ( "data-mutation", "still-on" )
            , labelAttributes =
                [ ( "role", "switch" )
                , ( "aria-checked", "true" )
                ]
            , labelSelector = ( "data-mutation", "label-still-on" )
            }

        SwitchedOff ->
            { childAttributes =
                [ ( "aria-hidden", "true" ) ]
            , childSelector = ( "data-mutation", "switched-off" )
            , labelAttributes =
                [ ( "role", "switch" )
                , ( "aria-checked", "false" )
                ]
            , labelSelector = ( "data-mutation", "label-switched-off" )
            }

        StillOff ->
            { childAttributes = []
            , childSelector =
                ( "data-mutation", "still-off" )
            , labelAttributes =
                [ ( "role", "switch" )
                , ( "aria-checked", "false" )
                ]
            , labelSelector = ( "data-mutation", "label-still-off" )
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
                    Wrapper region narrowMsg msg
                    -> Wrapper region narrowMsg msg
                addAttributes innerWrapper =
                    let
                        recurse : Html region narrowMsg msg -> Html region narrowMsg msg
                        recurse =
                            Ui.mapWrapper addAttributes
                    in
                    case innerWrapper of
                        Node config tagName attrs contingent ->
                            Node config tagName (attrs ++ attributes) contingent

                        Ol attrs contingent ->
                            Ol (attrs ++ attributes) contingent

                        Ul attrs contingent ->
                            Ul (attrs ++ attributes) contingent

                        KeyedNode tagName attrs keyedElements ->
                            KeyedNode tagName (attrs ++ attributes) keyedElements

                        Nested { regions, combine } ->
                            Nested { regions = regions, combine = combine }

                        Toggle attrs { flag, inHeader, label } contingent ->
                            Toggle
                                (attrs ++ staticAttributes)
                                { flag = flag, inHeader = inHeader, label = label }
                                (recurse contingent)

                        Filter category contingent ->
                            Filter
                                category
                                (contingent >> recurse)

                        Search category config contingent ->
                            Search
                                category
                                ((\a -> { a | attributes = a.attributes ++ staticAttributes }) config)
                                (contingent >> recurse)

                        GoTo attrs config contingent ->
                            GoTo (attrs ++ staticAttributes) config (recurse contingent)

                        Bounce attrs config contingent ->
                            Bounce (attrs ++ staticAttributes) config (recurse contingent)

                attributes : List (Html.Attribute msg)
                attributes =
                    List.map (Attr.map never) staticAttributes

                staticAttributes : List (Html.Attribute Never)
                staticAttributes =
                    childSelector
                        :: childAttributes
                        |> List.map (\( attributeName, attributeValue ) -> Attr.attribute attributeName attributeValue)

                { childAttributes, childSelector } =
                    getAttributes mutation
            in
            Ui.mapWrapper addAttributes

        getMutation : Link -> Maybe String -> Mutation
        getMutation =
            Link.mutationFromTwoStates states

        {- map the nested wrappers to appear/disappear -}
        labelAttributesByMutation : Mutation -> List (Html.Attribute msg)
        labelAttributesByMutation =
            getAttributes
                >> (\{ labelAttributes, labelSelector } -> labelSelector :: labelAttributes)
                >> List.map
                    (\( attributeName, attributeValue ) -> Attr.attribute attributeName attributeValue)

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
        Node { onlyInCurrentRegion } str attrs elements ->
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
                                    :: Attr.type_ "search"
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


{-| Add delightful css transitions to reflect the Url transitions.

Works best when little additional css is added to your app.

-}
animations : Html region_ msg_ narrowMsg_
animations =
    let
        animation : String -> List ( String, List ( String, String ) ) -> String
        animation name =
            List.map
                (\( position, rules ) ->
                    position
                        :: List.map
                            (\( key, value ) -> " {" ++ key ++ ": " ++ value ++ "}\n")
                            rules
                        |> String.concat
                )
                >> String.concat
                >> ruleset [ "@keyframes " ++ name ]

        blockElements : String
        blockElements =
            ":is(address, article, aside, blockquote, canvas, dd, div, dl, dt, fieldset, figcaption, figure, footer, form, h1, h2, h3, h4, h5, h6, header, hr, li, main, nav, ol, p, pre, section, table, tfoot, ul, video)"

        children : List Mutation -> String
        children =
            List.map
                (\mutation ->
                    let
                        ( attr, value ) =
                            (getAttributes mutation).childSelector
                    in
                    "[" ++ attr ++ "=\"" ++ value ++ "\"]"
                )
                >> either

        inlineElements : String
        inlineElements =
            selectNot blockElements

        selectNot : String -> String
        selectNot selector =
            ":not(" ++ selector ++ ")"

        labels : List Mutation -> String
        labels =
            List.map
                (\mutation ->
                    let
                        ( attr, value ) =
                            (getAttributes mutation).labelSelector
                    in
                    "[" ++ attr ++ "=\"" ++ value ++ "\"]"
                )
                >> either

        ruleset : List String -> String -> String
        ruleset selectors rule =
            String.concat selectors
                ++ "{\n"
                ++ rule
                ++ "\n}\n\n"

        either : List String -> String
        either selectors =
            ":is(" ++ String.join ", " selectors ++ ")"
    in
    [ animation "less-ui-blink"
        [ ( "90%", [ ( "filter", "invert(1)" ) ] ) ]
    , ruleset [ labels [ StateEntered "" ] ]
        """
            animation: 
                3 .1s alternate steps(1, start) .05s less-ui-blink;
        """

    ---- Block Elements
    -- Note that line height must be set because
    -- otherwise it's often `normal` and can't be
    -- animated. `normal` varies between 1 and more.
    , ruleset
        [ blockElements
        , children
            [ StateEntered ""
            , SwitchedOn
            , StateInside ""
            , StillOn
            ]
        ]
        """
            line-height: 1.15;
        """
    , ruleset
        [ blockElements
        , children
            [ StateEntered ""
            , SwitchedOn
            ]
        ]
        """
            transition: 
                .2s line-height,
                .2s margin,
                .2s padding,
                0s 0s height,
                0s 0s visibility;
        """
    , ruleset
        [ blockElements
        , children
            [ StateLeft ""
            , SwitchedOff
            , StateOutside ""
            , StillOff
            ]
        ]
        """
            line-height:0;
            margin:0; 
            padding:0;
            visibility: hidden;
        """
    , ruleset
        [ blockElements
        , children
            [ StateLeft ""
            , SwitchedOff
            ]
        ]
        """
            transition: 
                .2s line-height,
                .2s margin,
                .2s padding,
                0s .2s height,
                0s .2s visibility;
        """
    , ruleset
        [ blockElements
        , children
            [ StateOutside ""
            , StillOff
            ]
        ]
        """
            height:0
        """

    ---- Nasty Descendents
    , ruleset
        [ blockElements
        , children
            [ StateLeft ""
            , SwitchedOff
            , StateOutside ""
            , StillOff
            ]
        , " " ++ blockElements
        ]
        """
            margin:0; padding:0;
            
        """

    ---- Inline Elements
    , ruleset
        [ inlineElements
        , children
            [ StateEntered ""
            , SwitchedOn
            , StateInside ""
            , StillOn
            ]
        ]
        """
        """
    , ruleset
        [ inlineElements
        , children
            [ StateEntered ""
            , SwitchedOn
            ]
        ]
        """
            transition:
                .2s font-size,
                .2s text-indent,
                .2s padding,
                .1s opacity,
                0s 0s visibility;
        """
    , ruleset
        [ inlineElements
        , children
            [ StateLeft ""
            , SwitchedOff
            , StateOutside ""
            , StillOff
            ]
        ]
        """
            text-indent:0;
            font-size:0;
            padding:0;
            opacity:0;
            visibility: hidden;
        """
    , ruleset
        [ inlineElements
        , children
            [ StateLeft ""
            , SwitchedOff
            ]
        ]
        """
            transition: 
                .2s font-size,
                .2s text-indent,
                .2s padding,
                .1s .1s opacity,
                0s .2s visibility;
        """
    , ruleset
        [ inlineElements
        , children
            [ StateOutside ""
            , StillOff
            ]
        ]
        """
            opacity:0
        """
    ]
        |> List.map Html.text
        |> Ui.singleton
        |> node "style" []


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
