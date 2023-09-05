module Restrictive.Ui exposing
    ( Ui, Item
    , singleton, wrap
    , at
    , view
    , Layout, Wrapper(..)
    , repeat
    , uncons
    , map
    , indexedMapList, mapList
    , mapEach
    , mapWrapper
    )

{-| Separate [State](Ui.State) and [Layout](Ui.Layout) of interface elements from the main model
and build accessible patterns orthogonal to the Dom tree.

@docs Ui, Item


# Create

@docs singleton, wrap


# Modify

@docs at


# Append

**[`a ++ b`](https://package.elm-lang.org/packages/elm/core/latest/Basics#++)**


# View

@docs view

@docs Layout, Wrapper

---


# Convenience

@docs repeat

In addition, many List functions directly work with `Ui`, for example `List.length`, `List.reverse` or `List.Extra.permutations`.
Caveats are discussed in [Advanced Usage](advanced-usage)


### Decompose

@docs uncons


# Advanced Usage


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

import List.Extra as List
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout.Region exposing (OrHeader(..))
import Restrictive.State as State exposing (State)


{-| -}
type alias Ui region html wrapper =
    List (Item region html wrapper)


{-| -}
type Item region html wrapper
    = Leaf html
    | Wrap wrapper
    | At region (Ui region html wrapper)



---- CREATE ----


{-| -}
singleton : html -> Ui region_ html wrapper_
singleton =
    Leaf >> List.singleton


{-| Check out [the default wrappers in `Restrictive.Layout.Html`](Restrictive.Layout.Html#wrap-the-dom).
-}
wrap : wrapper -> Ui region_ html_ wrapper
wrap =
    Wrap >> List.singleton



---- MODIFY ----


{-| Designate a region for each descendant.

Note that the last designation "wins".

[#26: Once designated, `Ui`s can't be redesignated](https://github.com/upsiflu/restrictive/issues/26)

-}
at : region -> Ui region html wrapper -> Ui region html wrapper
at region =
    At region >> List.singleton



-- Functions inherited from List --


{-| Modify items `Ui`s according to their order. For example, zip their indices between the elements:

    singleton [1008] ++ singleton [2004] ++ singleton [1007]
        |> indexedMapList (\i -> (++) (singleton [i]))
        |> toList
            --> [0, 1008, 1, 2004, 2, 1007]

-}
indexedMapList : (Int -> Ui region html wrapper -> Ui region html wrapper) -> Ui region html wrapper -> Ui region html wrapper
indexedMapList fu =
    List.indexedMap (\i -> List.singleton >> fu i) >> List.concat


{-|

    repeat n =
        List.repeat n >> List.concat

-}
repeat : Int -> Ui region html wrapper -> Ui region html wrapper
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
mapList : (List (Ui region html wrapper) -> List (Ui region2 html2 wrapper2)) -> Ui region html wrapper -> Ui region2 html2 wrapper2
mapList fu =
    List.map List.singleton >> fu >> List.concat


{-| Modify each descendent as a separate Ui and then recombine them.

    region html attribute wrapper   "A" ++ region html attribute wrapper   "B" ++ region html attribute wrapper   "C"
        |> mapEach ((++) (html ", "))
        ---> something like A, B, C

-}
mapEach : (Ui region html wrapper -> Ui region2 html2 wrapper2) -> Ui region html wrapper -> Ui region2 html2 wrapper2
mapEach fu =
    List.concatMap (List.singleton >> fu)



---- MAP ----


{-| -}
map : (html -> html2) -> Ui region html wrapper -> Ui region html2 wrapper
map fu =
    List.map
        (\item ->
            case item of
                Leaf html ->
                    Leaf (fu html)

                Wrap wrapper ->
                    Wrap wrapper

                At innerRegion elements ->
                    At innerRegion (map fu elements)
        )


{-| -}
mapWrapper : (wrapper -> wrapper2) -> Ui region html wrapper -> Ui region html wrapper2
mapWrapper fu =
    List.map
        (\item ->
            case item of
                Leaf html ->
                    Leaf html

                Wrap wrapper ->
                    Wrap (fu wrapper)

                At innerRegion elements ->
                    At innerRegion (mapWrapper fu elements)
        )



---- DECOMPOSE ----


{-| Attempt to separate the first descendant in the Ui
-}
uncons : Ui region html wrapper -> Maybe ( Ui region html wrapper, Ui region html wrapper )
uncons =
    List.uncons >> Maybe.map (Tuple.mapFirst List.singleton)



---- LAYOUT ----


{-| The layout is a rule for mapping a Ui to an html tree.
-}
type alias Layout region narrowHtml html narrowWrapper customWrapper =
    { removed : html -> html
    , removable : html -> html
    , inserted : html -> html
    , wrap : customWrapper -> Wrapper region narrowHtml html narrowWrapper customWrapper
    , concat : List html -> html
    , arrange : Get (OrHeader region) html -> html
    }


{-| Create a custom `wrapper` to sneak functions into a
functionless `Ui`. At the `layout` phase, you decide what
your `wrapper` does:

  - `WrapHtml`: Transform the rendered `html` inside the current region.
  - `Nest`: Render a `Ui` with a different `html` type and then
    nest it in a supplied function.
    This transformation is applied parallelly within each region.
  - `Link`: Render a link. The Url determines whether the enclosed Ui
    is rendered or not.

You can either use the provided `Wrapper` or roll your own.
Advantage of rolling your own `wrapper` type: You don't need to store functions in the Ui,
which makes it comparable and serializable.

See [`Layout.Html`](Layout.Html#Wrapper) for the default example of a defunctionalized wrapper.

-}
type Wrapper region narrowHtml html narrowWrapper wrapper
    = WrapHtml (html -> html) (Ui region html wrapper)
    | Nest
        { regions : List region
        , narrowLayout : Layout region narrowHtml narrowHtml narrowWrapper narrowWrapper
        , combine : { makeInnerHtml : Ui region narrowHtml narrowWrapper -> Maybe narrowHtml } -> html
        }
    | Link (State.Templates html) (State.LinkStyle html) State.Link (Maybe State.LinkData -> Ui region html wrapper)
    | Keyed (List ( String, html ) -> html) (List ( String, Ui region html wrapper ))



---- VIEW ----


{-| -}
view : { current : State, previous : Maybe State } -> Layout region narrowHtml_ html narrowWrapper_ wrapper -> Ui region html wrapper -> html
view state layout =
    viewUi state layout Header
        >> layout.arrange


{-| Even though it's the same as `viewUi`, somehow Elm complains when I use `viewUi` inside `viewUi` with a different type.
I assume it's related to Elm not allowing cyclic type dependencies in `let` functions, let alone within functions.
So here is explicit polymorphism.
-}
viewOtherUi : { current : State, previous : Maybe State } -> Layout region narrowHtml_ html narrowWrapper_ wrapper -> OrHeader region -> Ui region html wrapper -> Get (OrHeader region) html
viewOtherUi =
    viewUi


viewUi :
    { current : State, previous : Maybe State }
    -> Layout region narrowHtml_ html narrowWrapper_ wrapper
    -> OrHeader region
    -> Ui region html wrapper
    -> Get (OrHeader region) html
viewUi state layout region =
    let
        viewItem : Item region html wrapper -> Get (OrHeader region) html
        viewItem item =
            case item of
                Leaf html_ ->
                    Get.singleton region html_

                Wrap wrapper ->
                    viewWrapper wrapper

                At innerRegion elements ->
                    viewUi state layout (Region innerRegion) elements

        viewWrapper : wrapper -> Get (OrHeader region) html
        viewWrapper wrapper =
            case layout.wrap wrapper of
                WrapHtml fu elements ->
                    viewUi state layout region elements
                        |> Get.updateAt region fu

                Nest { regions, narrowLayout, combine } ->
                    --Important: at this implementation, `Nest` throws away the elements!
                    List.map
                        (\soloRegion ->
                            ( soloRegion
                            , combine
                                { makeInnerHtml =
                                    (case region of
                                        Header ->
                                            identity

                                        Region r ->
                                            at r
                                    )
                                        >> viewOtherUi state narrowLayout soloRegion
                                        >> Get.get soloRegion
                                }
                            )
                        )
                        (Header :: List.map Region regions)
                        |> Get.fromList

                Link templates linkStyle link getElements ->
                    [ State.view
                        region
                        state.current
                        templates
                        linkStyle
                        link
                    , case
                        ( State.linkStatus state.current link
                        , Maybe.map (\s -> State.linkStatus s link) state.previous
                        , State.linkData state.current link
                        )
                      of
                        ( True, Just False, linkData ) ->
                            viewOtherUi state layout region (getElements linkData)
                                |> Get.map layout.inserted

                        ( True, _, linkData ) ->
                            viewUi state layout region (getElements linkData)
                                |> Get.map layout.removable

                        ( False, Just True, linkData ) ->
                            viewUi state layout region (getElements linkData)
                                |> Get.map layout.removed

                        ( False, _, _ ) ->
                            Get.empty
                    ]
                        |> Get.concatBy layout.concat

                Keyed fu elementLists ->
                    List.concatMap
                        (\( key, ui ) ->
                            List.indexedMap
                                (\i item ->
                                    viewItem item
                                        |> Get.map (\itm -> [ ( key ++ String.fromInt i, itm ) ])
                                )
                                ui
                        )
                        elementLists
                        |> Get.concat
                        |> Get.map fu
    in
    List.map viewItem
        >> Get.concatBy layout.concat
