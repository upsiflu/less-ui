module Restrictive.Layout.Html exposing
    ( Ui, singleton
    , toggle, goTo, bounce, filter
    , ol, ul, keyedNode, nest
    , layout
    , wrap, arrange, concat
    )

{-| Default types and functions for working with [elm/html](https://package.elm-lang.org/packages/elm/html/latest/) within [`Restrictive.Ui`](Restrictive.Ui)

@docs Ui, singleton


# Create Links

[Read more in the `Link` module.](Restrictive.Link)

@docs toggle, goTo, bounce, filter


# Wrap the DOM

@docs ol, ul, keyedNode, nest


# Layout

@docs layout


### Assemble your own layout record:

@docs wrap, arrange, concat

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed
import Html.Lazy
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout.Region as Region exposing (OrHeader(..), Region(..), withHeader)
import Restrictive.Link as Link exposing (Link, Mutation(..), State)
import Restrictive.Ui as Ui


{-| -}
type alias Ui narrowMsg msg =
    Ui.Ui
        Region
        (HtmlList (Link.Msg msg))
        (Wrapper narrowMsg msg)


{-| -}
type alias NarrowUi narrowMsg =
    Ui narrowMsg narrowMsg


{-| -}
singleton : List (Html msg) -> Ui narrowMsg_ msg
singleton =
    List.map (Html.map Link.AppMsg)
        >> Ui.singleton



---- Create links ----


{-| Toggle a `Flag` and show/hide the associated `Ui` accordingly.
Will add the flag when the link is opened in a new tab or shared, no matter its state in the current tab.
-}
toggle :
    List (Html.Attribute Never)
    ->
        { flag : Link.Flag
        , isInline : Bool
        , label : HtmlList msg
        }
    -> Ui narrowMsg msg
    -> Ui narrowMsg msg
toggle attributes config =
    Toggle attributes config
        >> Ui.wrap


{-| Filter or search a `Category`, then show a `Ui` according to the given `SearchTerm`(s).
-}
filter :
    List (Html.Attribute Never)
    ->
        { category : Link.Category
        , isInline : Bool
        , label : HtmlList msg
        }
    -> (List Link.SearchTerm -> Ui narrowMsg msg)
    -> Ui narrowMsg msg
filter attributes config =
    Filter attributes config
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
    -> Ui narrowMsg msg
    -> Ui narrowMsg msg
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
    -> Ui narrowMsg msg
    -> Ui narrowMsg msg
bounce attributes config =
    Bounce attributes config
        >> Ui.wrap



---- Working with contingent transformations ----


{-| -}
type Wrapper narrowMsg msg
    = Node String (List (Html.Attribute (Link.Msg msg))) (Ui narrowMsg msg)
    | Keyed (List ( String, HtmlList (Link.Msg msg) ) -> HtmlList (Link.Msg msg)) (List ( String, Ui narrowMsg msg ))
    | Nested
        { combine :
            { makeInnerHtml :
                NarrowUi narrowMsg
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
        (Ui narrowMsg msg)
    | Filter
        (List (Html.Attribute Never))
        { category : Link.Category
        , isInline : Bool
        , label : HtmlList msg
        }
        (List Link.SearchTerm -> Ui narrowMsg msg)
    | GoTo
        (List (Html.Attribute Never))
        { destination : Link.Location
        , isInline : Bool
        , label : HtmlList msg
        }
        (Ui narrowMsg msg)
    | Bounce
        (List (Html.Attribute Never))
        { there : Link.Location
        , here : Link.Location
        , label : HtmlList msg
        }
        (Ui narrowMsg msg)


type alias HtmlList msg =
    List (Html msg)


applyKeyedFu : (List ( String, Html msg ) -> Html msg) -> (List ( String, HtmlList msg ) -> HtmlList msg)
applyKeyedFu fu =
    List.concatMap
        (\( key, items ) ->
            List.indexedMap (\i a -> ( key ++ "." ++ String.fromInt i, a )) items
        )
        >> fu
        >> List.singleton


{-| -}
ol : List (Html.Attribute msg) -> List ( String, Ui narrowMsg msg ) -> Ui narrowMsg msg
ol attrs =
    Keyed (applyKeyedFu (Html.Keyed.ol (List.map (Attr.map Link.AppMsg) attrs))) >> Ui.wrap


{-| -}
ul : List (Html.Attribute msg) -> List ( String, Ui narrowMsg msg ) -> Ui narrowMsg msg
ul attrs =
    Keyed (applyKeyedFu (Html.Keyed.ul (List.map (Attr.map Link.AppMsg) attrs))) >> Ui.wrap


node : String -> List (Html.Attribute (Link.Msg msg)) -> Ui narrowMsg msg -> Ui narrowMsg msg
node str attrs =
    Node str attrs >> Ui.wrap


{-| -}
keyedNode : String -> List (Html.Attribute msg) -> List ( String, Ui narrowMsg msg ) -> Ui narrowMsg msg
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
    { combine :
        { makeInnerHtml :
            NarrowUi narrowMsg
            -> Maybe (HtmlList (Link.Msg narrowMsg))
        }
        -> HtmlList (Link.Msg msg)

    -- Plug it into your widget here
    }
    -> Ui narrowMsg msg
nest config =
    Ui.wrap (Nested config)



----- LAYOUT -----


{-| -}
layout :
    Ui.Layout
        Region
        (HtmlList (Link.Msg narrowMsg))
        (HtmlList (Link.Msg msg))
        (Wrapper narrowMsg narrowMsg)
        (Wrapper narrowMsg msg)
layout =
    { wrap = wrap
    , concat = concat
    , arrange = arrange
    }


{-| -}
wrap :
    { current : State, previous : Maybe State }
    -> Wrapper narrowMsg msg
    ->
        Ui.Wrapper
            Region
            (HtmlList (Link.Msg narrowMsg))
            (HtmlList (Link.Msg msg))
            (Wrapper narrowMsg narrowMsg)
            (Wrapper narrowMsg msg)
wrap states wrapper =
    let
        appHtml : HtmlList msg -> HtmlList (Link.Msg msg)
        appHtml =
            List.map (Html.map Link.AppMsg)

        attributesByMutation : Mutation -> List (Html.Attribute msg)
        attributesByMutation mutation =
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

        getMutation : Link.Link -> Maybe String -> Mutation
        getMutation =
            Link.mutationFromTwoStates states

        wrapByMutation : Mutation -> Ui narrowMsg msg -> Ui narrowMsg msg
        wrapByMutation mutation =
            case mutation of
                StateEntered _ ->
                    node "span" [ Attr.class "inserted removable" ]

                StateInside _ ->
                    node "span" [ Attr.class "removable" ]

                StateLeft _ ->
                    node "span" [ Attr.class "removed", Attr.attribute "aria-hidden" "true", Attr.tabindex -1 ]

                StateOutside _ ->
                    \_ -> node "span" [ Attr.class "removed", Attr.attribute "aria-hidden" "true", Attr.tabindex -1 ] []

                SwitchedOn ->
                    node "span" [ Attr.class "inserted removable" ]

                StillOn ->
                    node "span" [ Attr.class "removable" ]

                SwitchedOff ->
                    node "span" [ Attr.class "removed", Attr.attribute "aria-hidden" "true", Attr.tabindex -1 ]

                StillOff ->
                    \_ -> node "span" [ Attr.class "removed", Attr.attribute "aria-hidden" "true", Attr.tabindex -1 ] []
    in
    case wrapper of
        Node str attrs elements ->
            Ui.Wrapped (Html.node str attrs >> List.singleton) elements

        Keyed fu list ->
            Ui.Keyed fu list

        Nested { combine } ->
            Ui.Nested
                { regions = [ Scene, Control, Info ]
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
                                :: attributesByMutation mutation
                                ++ List.map (Attr.map never) attributes
                            )
                            label
                        ]
                , isInline = isInline
                , contingent =
                    wrapByMutation mutation contingent
                }

        Filter attributes { category, isInline, label } contingent ->
            let
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
                    appHtml label
                        ++ [ Html.input
                                (Attr.value (String.join " " searchTerms)
                                    :: Events.onInput
                                        (\newSearchTerm ->
                                            Link.UrlCmd (link newSearchTerm)
                                        )
                                    :: List.map (Attr.map Link.AppMsg) (Attr.title category :: attributesByMutation mutation)
                                    ++ List.map (Attr.map never) attributes
                                )
                                []
                           ]
                , isInline = isInline
                , contingent = wrapByMutation mutation (contingent searchTerms)
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
                                :: attributesByMutation mutation
                                ++ List.map (Attr.map never) attributes
                            )
                            label
                        ]
                , isInline = isInline
                , contingent =
                    wrapByMutation mutation contingent
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
                                :: attributesByMutation mutation
                                ++ List.map (Attr.map never) attributes
                            )
                            label
                        ]
                , isInline = False
                , contingent =
                    wrapByMutation mutation contingent
                }


{-| -}
concat : List (List a) -> List a
concat =
    List.concat


{-| -}
arrange : Get (OrHeader Region) (HtmlList msg) -> HtmlList msg
arrange =
    withHeader Region.allRegions
        |> Get.toListBy
            (Get.fromList
                [ ( Header
                  , Html.Lazy.lazy2 Html.header [ Attr.class "header" ]
                  )
                , ( Region Scene
                  , Html.Lazy.lazy2 Html.main_ [ Attr.class "scene" ]
                  )
                , ( Region Control
                  , Html.Lazy.lazy2 Html.div [ Attr.class "control" ]
                  )
                , ( Region Info
                  , Html.Lazy.lazy2 Html.div [ Attr.class "info" ]
                  )
                ]
            )
