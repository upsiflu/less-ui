module Restrictive.Ui exposing
    ( Ui, Item
    , singleton
    , bounce, goTo, toggle, customLink
    , wrap, at
    , view, toString, toList, toListString
    , repeat
    , ol, ul, node
    , uncons
    , stateful
    , StatelessWrapper
    , map
    , indexedMapList, mapList
    , mapEach
    , mapWrapper
    )

{-| Separate [State](Ui.State) and [Layout](Ui.Layout) of interface elements from the main model
and build accessible patterns orthogonal to the Dom tree.

Ui is headless (like elm-widgets will be).
Note that you can use `++`, `List.concatMap` and friends because `Ui`s are Lists.

@docs Ui, Item


# Create

@docs singleton

Create a **Link**, then `attach` dependent views:

@docs bounce, goTo, toggle, customLink


# Modify

@docs wrap, at


# Append

**[`a ++ b`](https://package.elm-lang.org/packages/elm/core/latest/Basics#++)**


# View

@docs view, toString, toList, toListString

---


# Convenience

@docs repeat

In addition, many List functions directly work with `Ui`, for example `List.length`, `List.reverse` or `List.Extra.permutations`.
Caveats are discussed in [Advanced Usage](advanced-usage)


### Keyed Wrappers

@docs ol, ul, node


### Decompose

@docs uncons


# Advanced Usage


## Mix with Html

@docs stateful
@docs StatelessWrapper


## Map

@docs map

Since `Ui`s are `List`s, it is easy to use the library functions from the `List` and `List.Extra` packages.
However, I recommend against it.

    {-| Combines items from two `Ui`s. If one `Ui` is longer, its excessive elements are dropped.
    -}
    map2 fu a b =
        List.map2 fu (List.map List.singleton a) (List.map List.singleton b)
            |> List.concat

    map3 fu a b c =
        List.map3 fu
            (List.map List.singleton a)
            (List.map List.singleton b)
            (List.map List.singleton c)
            |> List.concat

The big drawback when using `Ui`s as `List`s is that you cannot inspect (compare, filter, sort) them because
the `Item` type is opaque.
It is usually easier to build exactly the `Ui` you need instead of altering and recombining them after the fact.

@docs indexedMapList, mapList
@docs mapEach


# Slated for removal

The following exports have no application and may be removed in the next release.

@docs mapWrapper

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Keyed
import List.Extra as List
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout as Layout exposing (Layout)
import Restrictive.Layout.Html as Html
import Restrictive.Layout.Region exposing (OrHeader(..), Region)
import Restrictive.State as State exposing (State)


{-| -}
type alias Ui region html attribute wrapper =
    List (Item region html attribute wrapper)


{-|

    Leaf html           -- Rendered html node

    Twig State.LinkStyle
         State.Link
         Ui             -- Logical bifurcation (link*state active/inactive)

    At region Ui        -- Visual bifurcation (screen regions)

    Wrap wrapper Ui     -- Dom bifurcation (things like section, header, p, ul, table...)

    Stateful (List region)
        { makeOuterHtml :
            { makeInnerHtml : Ui -> Html Never }
            -> html
        }               -- Composes nested `view` functions from libraries such as elm-any-type-form
                        -- that require a specific `Html x` (Html delta) type for custom views.
                        -- Note that this means that a form's Html can never nest another form's Html
                        -- because forms rely on messages, and views can only emit their local delta.

-}
type Item region html attribute wrapper
    = Leaf html
    | Twig (State.LinkStyle html attribute) State.Link (Ui region html attribute wrapper)
    | At region (Ui region html attribute wrapper)
    | Wrap wrapper (Ui region html attribute wrapper)
    | Stateful
        (List region)
        (Layout region (List (Html Never)) (Html.Attribute Never) StatelessWrapper)
        ({ makeInnerHtml :
            Ui region (List (Html Never)) (Html.Attribute Never) StatelessWrapper -> List (Html Never)
         }
         -> html
        )



---- CREATE ----


{-| -}
singleton : html -> Ui region_ html attribute_ wrapper_
singleton =
    Leaf >> List.singleton



---- MODIFY ----


{-| -}
wrap : wrapper -> Ui region html attribute wrapper -> Ui region html attribute wrapper
wrap wrapper =
    Wrap wrapper >> List.singleton



---- COMPOSE ----


{-| Logically nest sub-Ui to each descendant.

If any logical parent is a link pointing away from the current state, this sub-Ui will be hidden.

Note that only links will receive members! Simple `singleton` Html nodes will be unaffected.

(Later, we can add a phantom type to make sure we can only call `with` on Uis that contain a Twig)

    singleton 1 |> with (singleton 2)
        -> [Leaf 1]

-}
with : Ui region html attribute wrapper -> Ui region html attribute wrapper -> Ui region html attribute wrapper
with members_ =
    List.map
        (\original ->
            case original of
                Twig linkStyle link members ->
                    Twig linkStyle link (members ++ members_)

                At region ui ->
                    At region (with members_ ui)

                Wrap wrapper ui ->
                    Wrap wrapper (with members_ ui)

                _ ->
                    original
        )


{-| Designate a region for each descendant.

Note that the last designation "wins".

Use `ui0 |> (with << at Region0) ui1` to logically nest ui1 under ui0 and designate ui1 to Region0.

Todo: once designated, `Ui`s can't be redesignated

-}
at : region -> Ui region html attribute wrapper -> Ui region html attribute wrapper
at region =
    At region >> List.singleton



---- Keyed Html ----


type alias KeyedUi region msg =
    Ui region (List ( String, Html msg )) (Html.Attribute msg) (List ( String, Html msg ) -> List ( String, Html msg ))


{-| convenience function to wrap a Ui into an unordered list and give it an id
-}
ul : String -> KeyedUi region msg -> KeyedUi region msg
ul idString =
    wrap (Html.Keyed.ul [ Attr.id idString ] >> Tuple.pair idString >> List.singleton)


{-| convenience function to wrap a Ui into an ordered list and give it an id
-}
ol : String -> KeyedUi region msg -> KeyedUi region msg
ol idString =
    wrap (Html.Keyed.ul [ Attr.id idString ] >> Tuple.pair idString >> List.singleton)


{-| convenience function to wrap a Ui into any Html node and give it an id
-}
node : String -> String -> KeyedUi region msg -> KeyedUi region msg
node nodeType idString =
    wrap (Html.Keyed.node nodeType [ Attr.id idString ] >> Tuple.pair idString >> List.singleton)



-- Functions inherited from List --


{-| Modify items `Ui`s according to their order. For example, zip their indices between the elements:

    singleton [1008] ++ singleton [2004] ++ singleton [1007]
        |> indexedMapList (\i -> (++) (singleton [i]))
        |> toList
            --> [0, 1008, 1, 2004, 2, 1007]

-}
indexedMapList : (Int -> Ui region html attribute wrapper -> Ui region html attribute wrapper) -> Ui region html attribute wrapper -> Ui region html attribute wrapper
indexedMapList fu =
    List.indexedMap (\i -> List.singleton >> fu i) >> List.concat


{-|

    repeat n =
        List.repeat n >> List.concat

-}
repeat : Int -> Ui region html attribute wrapper -> Ui region html attribute wrapper
repeat n =
    List.repeat n >> List.concat


{-|

    List.repeat 10 ()
        |> indexedMapList (\i _ -> textLabel (String.fromInt i))
        |> mapList (List.intersperse (textLabel "and"))

You may find it handy to use functions such as `List.Extra.setAt` to replace the n-th descendant in a List:

    import List.Extra as List

    textLabel "A" ++ textLabel "oops" ++ textLabel "C"
        |> mapList (List.setAt 1 (textLabel "the other B"))

or to remove a descendant:

    textLabel "A"
        ++ textLabel "oops"
        ++ textLabel "C"
        |> mapList (List.remove 1)

-}
mapList : (List (Ui region html attribute wrapper) -> List (Ui region2 html2 attribute2 wrapper2)) -> Ui region html attribute wrapper -> Ui region2 html2 attribute2 wrapper2
mapList fu =
    List.map List.singleton >> fu >> List.concat


{-| Modify each descendent as a separate Ui and then recombine them.

    region html attribute wrapper   "A" ++ region html attribute wrapper   "B" ++ region html attribute wrapper   "C"
        |> mapEach ((++) (html ", "))
        ---> something like A, B, C

-}
mapEach : (Ui region html attribute wrapper -> Ui region2 html2 attribute2 wrapper2) -> Ui region html attribute wrapper -> Ui region2 html2 attribute2 wrapper2
mapEach fu =
    List.concatMap (List.singleton >> fu)



---- MAP ----


{-| -}
map : (html -> html2) -> Ui region html attribute wrapper -> Ui region html2 attribute wrapper
map fu =
    List.map
        (\item ->
            case item of
                Leaf html ->
                    Leaf (fu html)

                Twig linkStyle link elements ->
                    Twig (State.mapLinkStyle fu linkStyle) link (map fu elements)

                At innerRegion elements ->
                    At innerRegion (map fu elements)

                Wrap wrapper elements ->
                    Wrap wrapper (map fu elements)

                Stateful regions layout makeOuterHtml ->
                    Stateful regions
                        layout
                        (makeOuterHtml >> fu)
        )


{-| -}
mapWrapper : (wrapper -> wrapper2) -> Ui region html attribute wrapper -> Ui region html attribute wrapper2
mapWrapper fu =
    List.map
        (\item ->
            case item of
                Leaf html ->
                    Leaf html

                Twig linkStyle link elements ->
                    Twig linkStyle link (mapWrapper fu elements)

                At innerRegion elements ->
                    At innerRegion (mapWrapper fu elements)

                Wrap wrapper elements ->
                    Wrap (fu wrapper) (mapWrapper fu elements)

                Stateful regions layout makeOuterHtml ->
                    Stateful regions layout makeOuterHtml
        )



---- DECOMPOSE ----


{-| Attempt to separate the first descendant in the Ui
-}
uncons : Ui region html attribute wrapper -> Maybe ( Ui region html attribute wrapper, Ui region html attribute wrapper )
uncons =
    List.uncons >> Maybe.map (Tuple.mapFirst List.singleton)



---- VIEW ----


{-| -}
view : State -> Layout region html attribute wrapper -> Ui region html attribute wrapper -> html
view state layout =
    viewUi state layout Header
        >> layout.arrange


{-| Even though it's the same as `viewUi`, somehow Elm complains when I use `viewUi` inside `viewUi` with a different type.
I assume it's related to Elm not allowing cyclic type dependencies in `let` functions, let alone within functions.
So here is explicit polymorphism.
-}
viewStatic : State -> Layout region (List (Html Never)) (Html.Attribute Never) wrapper -> OrHeader region -> Ui region (List (Html Never)) (Html.Attribute Never) wrapper -> Get (OrHeader region) (List (Html Never))
viewStatic =
    viewUi


viewUi : State -> Layout region html attribute wrapper -> OrHeader region -> Ui region html attribute wrapper -> Get (OrHeader region) html
viewUi state layout region =
    List.map
        (\item ->
            case item of
                Leaf html_ ->
                    Get.singleton region html_

                Twig linkStyle link elements ->
                    [ State.view
                        region
                        state.current
                        layout.elements
                        linkStyle
                        link
                    , case State.toTuple (State.linkIsActive link state) of
                        ( True, Just False ) ->
                            viewUi state layout region elements
                                |> Get.map (layout.wrap layout.inserted)

                        ( True, _ ) ->
                            viewUi state layout region elements
                                |> Get.map (layout.wrap layout.removable)

                        ( False, Just False ) ->
                            Get.empty

                        ( False, _ ) ->
                            viewUi state layout region elements
                                |> Get.map (layout.wrap layout.removed)
                    ]
                        |> Get.concatBy layout.concat

                At innerRegion elements ->
                    viewUi state layout (Region innerRegion) elements

                Wrap wrapper elements ->
                    viewUi state layout region elements
                        |> Get.updateAt region (layout.wrap wrapper)

                Stateful regions staticLayout makeOuterHtml ->
                    (Header :: List.map Region regions)
                        |> List.map
                            (\soloRegion ->
                                ( soloRegion
                                , makeOuterHtml
                                    { makeInnerHtml =
                                        (case region of
                                            Header ->
                                                identity

                                            Region r ->
                                                at r
                                        )
                                            >> viewStatic state staticLayout soloRegion
                                            >> Get.get soloRegion
                                            >> Maybe.withDefault [ Html.text "" ]
                                            >> List.map (Html.map never)
                                    }
                                )
                            )
                        |> Get.fromList
         {- Problem: The `outside` elements never receive the state that the `inside`
            elements receive.

            The interesting detail is how regions work here:
            1. Generate the inner Html, with the state information hidden inside the form
            2. Supply back the outer Html
            3. Set it as a child node at `region`
            4. concat it with all the other `elements`
               (which have no clue about the state inside the form)

            Solution:
              For each region, generate an outer Html,
              where each time the inner Html is limited to the same region.

            Example:
              We want a form that displays an inline input plus a hint at `info`.
              So for the inline region, the `.inside` will return the input;
              for the `Scene` and `Header it will return Html.none or [];
              and for `Info` it will return the hint.

            Implementation:
            1. We have the same `toHtml` but we use only `inRegion`.
            2. For each region, we render the whole thing:

                regions
                    |> List.map
                        \innerRegion ->
                            makeOuterHtml
                                { makeInnerHtml =
                                        at region
                                            >> viewUi innerRegion
                                            >> Get.get innerRegion
                                }
                                |> Maybe.map (Tuple.pair innerRegion)
                    |> Maybe.values
                    |> Get.fromList

            4. To make it somewhat more performant, we can
               add a parameter to `viewUi` to `onlyIncludeRegion`
               such that leaves at excluded regions are ignored
               and wraps at excluded regions don't update.


            New problem:

            `makeInnerHtml` is called in a context where `Html String` is needed,
            not `Html msg`.

            Solution:

            `makeInnerHtml` creates `Html Never` which we can convert to anything
            with `Html.map never`. Ta-da!


            New new problems:

            1.
            If we provide a `Ui` to `makeInnerHtml`, then this Ui must
            have stateless html, which means `Html Never`.
            But the `attribute` and `wrapper` type parameters are
            bound to that Attribute `Never`.

            2.
            Inside a `Form`, we want to construct `Html delta`,
            which is specific to the form's control.

         -}
        )
        >> Get.concatBy layout.concat


{-| For testing
-}
toString : Ui Region String String () -> String
toString =
    State.fromString "http://x/path_query"
        |> Maybe.map
            (\state ->
                view state Layout.textual
            )
        |> Maybe.withDefault (\_ -> "Failed to generate mock state")


{-| For testing
-}
toList : Ui region_ (List element) attribute_ () -> List element
toList =
    State.fromString "http://x/path_query"
        |> Maybe.map
            (\state ->
                view state (Layout.list_ List.concat)
            )
        |> Maybe.withDefault (\_ -> [])


{-| For testing
-}
toListString : Ui region_ String attribute_ () -> String
toListString =
    State.fromString "http://x/path_query"
        |> Maybe.map
            (\state ->
                String.join "; "
                    |> Layout.list_
                    |> view state
            )
        |> Maybe.withDefault (\_ -> "Failed to generate mock state")



---- Working with contingent transformations ----


{-| -}
customLink : State.LinkStyle html attribute -> State.Link -> Ui region_ html attribute wrapper_
customLink linkStyle link =
    [ Twig linkStyle link [] ]


{-| -}
toggle :
    List attribute
    ->
        { flag : State.Flag
        , isInline : Bool
        , label : html
        }
    -> Ui aspect html attribute wrapper
    -> Ui aspect html attribute wrapper
toggle attrs { flag, isInline, label } dependentUi =
    State.toggle flag
        |> customLink
            { attributes = attrs
            , isInline = isInline
            , label = label
            }
        |> with dependentUi


{-| -}
goTo :
    List attribute
    ->
        { destination : ( Maybe State.Path, State.Fragment )
        , isInline : Bool
        , label : html
        }
    -> Ui aspect html attribute wrapper
    -> Ui aspect html attribute wrapper
goTo attrs { destination, isInline, label } dependentUi =
    State.goTo destination
        |> customLink
            { attributes = attrs
            , isInline = isInline
            , label = label
            }
        |> with dependentUi


{-| -}
bounce :
    List attribute
    ->
        { here : ( Maybe State.Path, State.Fragment )
        , label : html
        , there : ( Maybe State.Path, State.Fragment )
        }
    -> Ui aspect html attribute wrapper
    -> Ui aspect html attribute wrapper
bounce attrs { here, label, there } dependentUi =
    State.bounce { here = here, there = there }
        |> customLink
            { attributes = attrs
            , isInline = True
            , label = label
            }
        |> with dependentUi


{-| Gives your Html widgets access to state information.

For example, if you want to extend a widget or form generator (`elm-any-type-forms`) that can only output Html
with `Ui` elements that alter and respond to the Url, then you need

  - a way to convert from `Ui` to `html` -> `view`
  - a way to convert from `html` to `Ui` -> `singleton`
  - a way to forward the current state to the nested `Ui`

Here is how you use this function:

1.  Write the `Ui` code for your widget extension.
    You can use all the local parameters your widget provides.

2.  Convert it to `html` using the `makeInnerHtml` function. Just pretend it exists:

    { makeInnerHtml } ->
    Ui.singleton...
    |> makeInnerHtml

    Note that the inner html will not bubble any messages to your app, so you are limited to Url-based state.

3.  Your widget will create `Html msg`. This will be the parameter you provide `stateful`.
    It will need a `Layout` to render `Ui (Html Never)` into `Html Never`.

Caution: If you use this functionality, the `Ui` will contain functions and will no longer support equality checks and serialisation.

-}
stateful :
    List region
    -> Layout region (List (Html Never)) (Html.Attribute Never) StatelessWrapper
    -> ({ makeInnerHtml : Ui region (List (Html Never)) (Html.Attribute Never) StatelessWrapper -> List (Html Never) } -> html)
    -> Ui region html attribute_ wrapper_
stateful regions layout makeOuterHtml =
    [ Stateful regions layout makeOuterHtml ]


type alias StatelessWrapper =
    Html.Wrapper Never
