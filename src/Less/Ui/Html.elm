module Less.Ui.Html exposing
    ( Ui, singleton
    , toggle, goTo, bounce, filter, search
    , section, node
    , ol, ul, keyedNode, nest
    , layout, arrangeOverDefaultRegions, Region(..)
    )

{-| Default types and functions for working with [elm/html](https://package.elm-lang.org/packages/elm/html/latest/) within [`Less.Ui`](Less.Ui)

@docs Ui, singleton


# Create Links

[Read more in the `Link` module.](Less.Link)

@docs toggle, goTo, bounce, filter, search


# Wrap the DOM

@docs section, node
@docs ol, ul, keyedNode, nest


# Layout

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


{-| -}
type alias Ui region narrowMsg msg =
    Ui.Ui
        region
        (HtmlList (Link.Msg msg))
        (Wrapper region narrowMsg msg)


{-| -}
type alias NarrowUi region narrowMsg =
    Ui region narrowMsg narrowMsg


{-| -}
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
    = Node { onlyInCurrentRegion : Bool } String (List (Html.Attribute (Link.Msg msg))) (Ui region narrowMsg msg)
    | Keyed (List ( String, HtmlList (Link.Msg msg) ) -> HtmlList (Link.Msg msg)) (List ( String, Ui region narrowMsg msg ))
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


{-| -}
section : List (Html.Attribute msg) -> Ui region narrowMsg msg -> Ui region narrowMsg msg
section attrs =
    node "section" (List.map (Attr.map Link.AppMsg) attrs)


{-| -}
node : String -> List (Html.Attribute (Link.Msg msg)) -> Ui region narrowMsg msg -> Ui region narrowMsg msg
node str attrs =
    Node { onlyInCurrentRegion = True } str attrs >> Ui.wrap


nodeWithConfig : { onlyInCurrentRegion : Bool } -> String -> List (Html.Attribute (Link.Msg msg)) -> Ui region narrowMsg msg -> Ui.Ui region_ html_ (Wrapper region narrowMsg msg)
nodeWithConfig config str attrs =
    Node config str attrs >> Ui.wrap


applyKeyedFu : (List ( String, Html msg ) -> Html msg) -> (List ( String, HtmlList msg ) -> HtmlList msg)
applyKeyedFu fu =
    List.concatMap
        (\( key, items ) ->
            List.indexedMap (\i a -> ( key ++ "." ++ String.fromInt i, a )) items
        )
        >> fu
        >> List.singleton


{-| -}
ol : List (Html.Attribute msg) -> List ( String, Ui region narrowMsg msg ) -> Ui region narrowMsg msg
ol attrs =
    Keyed (applyKeyedFu (Html.Keyed.ol (List.map (Attr.map Link.AppMsg) attrs))) >> Ui.wrap


{-| -}
ul : List (Html.Attribute msg) -> List ( String, Ui region narrowMsg msg ) -> Ui region narrowMsg msg
ul attrs =
    Keyed (applyKeyedFu (Html.Keyed.ul (List.map (Attr.map Link.AppMsg) attrs))) >> Ui.wrap


{-| -}
keyedNode : String -> List (Html.Attribute msg) -> List ( String, Ui region narrowMsg msg ) -> Ui region narrowMsg msg
keyedNode tagName attrs =
    Keyed (applyKeyedFu (Html.Keyed.node tagName (List.map (Attr.map Link.AppMsg) attrs))) >> Ui.wrap


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
        appHtml : HtmlList msg -> HtmlList (Link.Msg msg)
        appHtml =
            List.map (Html.map Link.AppMsg)

        getMutation : Link.Link -> Maybe String -> Mutation
        getMutation =
            Link.mutationFromTwoStates states

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

        wrapByMutation : { onlyInCurrentRegion : Bool } -> Mutation -> Ui region narrowMsg msg -> Ui region narrowMsg msg
        wrapByMutation config mutation =
            let
                animated : List (Html.Attribute (Link.Msg msg))
                animated =
                    [ Attr.style "transition" "font-size .2s 0s, opacity .2s .1s" ]

                hidden : List (Html.Attribute (Link.Msg msg))
                hidden =
                    [ Attr.attribute "aria-hidden" "true"
                    , Attr.tabindex -1
                    , Attr.style "opacity" "0"
                    , Attr.style "pointer-events" "none;"
                    , Attr.style "font-size" "0"
                    ]
            in
            case mutation of
                StateEntered _ ->
                    nodeWithConfig config "span" (Attr.class "state-entered" :: animated)

                StateInside _ ->
                    nodeWithConfig config "span" (Attr.class "state-inside" :: animated)

                StateLeft _ ->
                    nodeWithConfig config "span" (Attr.class "state-left" :: hidden ++ animated)

                StateOutside _ ->
                    nodeWithConfig config "span" (Attr.class "state-outside" :: hidden ++ animated)

                SwitchedOn ->
                    nodeWithConfig config "span" (Attr.class "switched-on" :: animated)

                StillOn ->
                    nodeWithConfig config "span" (Attr.class "still-on" :: animated)

                SwitchedOff ->
                    nodeWithConfig config "span" (Attr.class "switched-off" :: hidden ++ animated)

                StillOff ->
                    nodeWithConfig config "span" (Attr.class "still-off" :: hidden ++ animated)
    in
    case wrapper of
        Node config str attrs elements ->
            Ui.Wrapped config (Html.node str attrs >> List.singleton) elements

        Keyed fu list ->
            Ui.Keyed fu list

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
            Ui.Stateful
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
                , contingent =
                    wrapByMutation { onlyInCurrentRegion = isInline } mutation contingent
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
            Ui.Stateful
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
                                            :: List.map (Attr.map Link.AppMsg) (Attr.title category :: labelAttributesByMutation mutation)
                                            ++ List.map (Attr.map never) attributes
                                        )
                                        []
                                   ]

                        Nothing ->
                            []
                , isInline = isInline
                , contingent = wrapByMutation { onlyInCurrentRegion = isInline } mutation (contingent searchTerms)
                }

        GoTo attributes { destination, isInline, label } contingent ->
            let
                mutation : Mutation
                mutation =
                    getMutation link (Just "page")

                link : Link
                link =
                    Link.GoTo (Link.parseLocation destination)
            in
            Ui.Stateful
                { label =
                    appHtml
                        [ Html.a
                            (Attr.href destination
                                :: labelAttributesByMutation mutation
                                ++ List.map (Attr.map never) attributes
                            )
                            label
                        ]
                , isInline = isInline
                , contingent =
                    wrapByMutation { onlyInCurrentRegion = False } mutation contingent
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
            Ui.Stateful
                { label =
                    appHtml
                        [ Html.a
                            (Attr.href (Link.toHref link)
                                :: labelAttributesByMutation mutation
                                ++ List.map (Attr.map never) attributes
                            )
                            label
                        ]
                , isInline = False
                , contingent =
                    wrapByMutation { onlyInCurrentRegion = False } mutation contingent
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
