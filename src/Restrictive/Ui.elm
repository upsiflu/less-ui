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
    , indexedMapList, mapList
    , mapEach
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


## Map

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

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Keyed
import List.Extra as List
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout as Layout exposing (Layout)
import Restrictive.Layout.Region exposing (OrHeader(..), Region)
import Restrictive.State as State exposing (State)


{-| -}
type alias Ui region html attribute wrapper =
    List (Item region html attribute wrapper)


{-|

    Logical bifurcation (link*state active/inactive): `Twig`
    Visual bifurcation (screen regions): `At`

-}
type Item region html attribute wrapper
    = Leaf html
    | Twig (List attribute) (State.LinkStyle html) State.Link (Ui region html attribute wrapper)
    | At region (Ui region html attribute wrapper)
    | Wrap wrapper (Ui region html attribute wrapper)
    | Stateful (State -> html)



---- CREATE ----


{-| -}
singleton : html -> Ui region html attribute wrapper
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
                Leaf html ->
                    Leaf html

                Twig attrs linkStyle link members ->
                    Twig attrs linkStyle link (members ++ members_)

                At region ui ->
                    At region (with members_ ui)

                Wrap wrapper ui ->
                    Wrap wrapper (with members_ ui)

                Stateful fu ->
                    Stateful fu
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



---- DECOMPOSE ----


{-| Attempt to separate the first descendant in the Ui
-}
uncons : Ui region html attribute wrapper -> Maybe ( Ui region html attribute wrapper, Ui region html attribute wrapper )
uncons =
    List.uncons >> Maybe.map (Tuple.mapFirst List.singleton)



--idea: we want to create custom views for our forms (elm-any-type-form).
--they only accept Html msg as a type.
--so we have to render the custom view with a given state, then render the form with it, and then `singleton` the whole thing into a Ui again.
--Should be possible.
---- VIEW ----


{-| -}
view : State -> Layout region html attribute wrapper -> Ui region html attribute wrapper -> html
view state layout =
    let
        viewUi : OrHeader region -> Ui region html attribute wrapper -> Get (OrHeader region) html
        viewUi region =
            List.map
                (\descendant ->
                    case descendant of
                        Leaf html_ ->
                            Get.singleton region html_

                        Twig attrs linkStyle link elements ->
                            [ State.view region state.current layout.elements linkStyle attrs link
                            , case State.toTuple (State.linkIsActive link state) of
                                ( True, Just False ) ->
                                    viewUi region elements
                                        |> Get.map (layout.wrap layout.inserted)

                                ( True, _ ) ->
                                    viewUi region elements
                                        |> Get.map (layout.wrap layout.removable)

                                ( False, Just False ) ->
                                    Get.empty

                                ( False, _ ) ->
                                    viewUi region elements
                                        |> Get.map (layout.wrap layout.removed)
                            ]
                                |> Get.concatBy layout.concat

                        At innerRegion elements ->
                            viewUi (Region innerRegion) elements

                        Wrap wrapper elements ->
                            viewUi region elements
                                |> Get.updateAt region (layout.wrap wrapper)

                        Stateful fu ->
                            Get.singleton region (fu state)
                )
                >> Get.concatBy layout.concat
    in
    viewUi Header >> layout.arrange


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
toList : Ui region (List element) attribute () -> List element
toList =
    State.fromString "http://x/path_query"
        |> Maybe.map
            (\state ->
                view state (Layout.list_ List.concat)
            )
        |> Maybe.withDefault (\_ -> [])


{-| For testing
-}
toListString : Ui region String attribute () -> String
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
customLink : List attribute -> State.LinkStyle html -> State.Link -> Ui region html attribute wrapper
customLink attrs linkStyle link =
    [ Twig attrs linkStyle link [] ]


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
        |> customLink attrs
            { isInline = isInline
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
        |> customLink attrs
            { isInline = isInline
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
        |> customLink attrs
            { isInline = True
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
2.  Convert it to `html` using the `view` function. It will need a `Layout` which you can choose freely, and a State, which for now remains
    your additional function parameter.
3.  You will end up with a `Restrictive.State -> widget` function. Using your widget library's Html renderer, you can `>>` compose it with
    `widget -> html` and thus get the parameter for `stateful`.
4.  Now you have a `Ui` you can freely combine with the rest of your app.

Caution: If you use this functionality, the `Ui` will contain functions and will no longer support equality checks and serialisation.

-}
stateful : (State -> html) -> Ui region html attribute wrapper
stateful fu =
    [ Stateful fu ]
