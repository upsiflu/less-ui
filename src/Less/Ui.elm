module Less.Ui exposing
    ( Ui, Item
    , singleton, wrap
    , inRegion
    , view
    , Layout, CurrentLayout, Wrapper(..), applyStates
    , repeat
    , uncons
    , OrHeader(..)
    , map
    , indexedMapList
    , mapEach
    , mapWrapper
    )

{-| Separate [State](Less-Link#State) and [Layout](#Layout) of interface elements from the main model
and build accessible patterns orthogonal to the Dom tree.

@docs Ui, Item


# Create

@docs singleton, wrap


# Modify

@docs inRegion


# Append

**[`a ++ b`](https://package.elm-lang.org/packages/elm/core/latest/Basics#++)**


# Apply state

@docs view

@docs Layout, CurrentLayout, Wrapper, applyStates

---


# Convenience

@docs repeat

In addition, many List functions directly work with `Ui`, for example `List.length`, `List.reverse` or `List.Extra.permutations`.
Caveats are discussed in [Advanced Usage](#advanced-usage)


### Decompose

@docs uncons


# Working with Regions

@docs OrHeader

---


# Advanced Usage

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

@docs indexedMapList
@docs mapEach


# Slated for removal

The following exports have no application and may be removed in the next release.

@docs mapWrapper

-}

import AssocList as Dict exposing (Dict)
import Less.Link exposing (State)
import List.Extra as List
import Maybe.Extra as Maybe


{-|

    none : Ui region html wrapper
    none =
        []

-}
type alias Ui region html wrapper =
    List (Item region html wrapper)


{-| 🐚
-}
type Item region html wrapper
    = Leaf html
    | Wrap wrapper
    | At region (Ui region html wrapper)



---- CREATE ----


{-| 🐌
-}
singleton : html -> Ui region_ html wrapper_
singleton =
    Leaf >> List.singleton


{-| Check out [the default wrappers in `Less.Ui.Html`](Less-Ui-Html#wrap-the-dom).
-}
wrap : wrapper -> Ui region_ html_ wrapper
wrap =
    Wrap >> List.singleton



---- MODIFY ----


{-| Designate a region for each descendant.

Note that the last designation "wins".

[#26: Once designated, `Ui`s can't be redesignated](https://github.com/upsiflu/restrictive/issues/26)

-}
inRegion : region -> Ui region html wrapper -> Ui region html wrapper
inRegion region =
    At region >> List.singleton



-- Functions inherited from List --


{-| Modify items `Ui`s according to their order. For example, zip their indices between the elements:

    singleton [1008] ++ singleton [2004] ++ singleton [1007]
        |> indexedMapList (\i -> (++) (singleton [i]))
        |> List.length
            --> 6

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


{-| Modify each descendent as a separate Ui and then recombine them.

    region html attribute wrapper   "A" ++ region html attribute wrapper   "B" ++ region html attribute wrapper   "C"
        |> mapEach ((++) (html ", "))
        ---> something like A, B, C

-}
mapEach : (Ui region html wrapper -> Ui region2 html2 wrapper2) -> Ui region html wrapper -> Ui region2 html2 wrapper2
mapEach fu =
    List.concatMap (List.singleton >> fu)



---- MAP ----


{-| Modify the type of `html` in the Ui.

⚠️ If you can, build up your Ui with the final `html` type.

-- Todo: Tail Call Optimize.

-}
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
                    -- Todo: Tail Call Optimize.
                    At innerRegion (map fu elements)
        )


{-| Modify the type of `wrapper` in the Ui.

⚠️ If you can, build up your Ui with the final `wrapper` type.

-}
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
                    -- Todo: Tail Call Optimize.
                    At innerRegion (mapWrapper fu elements)
        )



---- DECOMPOSE ----


{-| Attempt to separate the first descendant in the Ui.
-}
uncons : Ui region html wrapper -> Maybe ( Ui region html wrapper, Ui region html wrapper )
uncons =
    List.uncons >> Maybe.map (Tuple.mapFirst List.singleton)



---- LAYOUT ----


{-| The layout is a rule for mapping a Ui to an `html` tree.
-}
type alias Layout region narrowHtml html narrowWrapper customWrapper =
    { wrap : { current : State, previous : Maybe State } -> customWrapper -> Wrapper region narrowHtml html narrowWrapper customWrapper
    , concat : List html -> html
    , arrange : { header : html, region : region -> html } -> html
    }


{-| fix a Layout to the current states to make it ["current"](#CurrentLayout).
-}
applyStates :
    { current : State, previous : Maybe State }
    -> Layout region narrowHtml html narrowWrapper customWrapper
    -> CurrentLayout region narrowHtml html narrowWrapper customWrapper
applyStates states layout =
    { wrap = layout.wrap states
    , concat = layout.concat
    , concatDicts = dict.concatBy layout.concat
    , arrange = layout.arrange
    }


{-| This type of Layout has its current and previous state already applied
through [applyStates](#applyStates).
-}
type alias CurrentLayout region narrowHtml html narrowWrapper customWrapper =
    { wrap : customWrapper -> Wrapper region narrowHtml html narrowWrapper customWrapper
    , concat : List html -> html
    , concatDicts : List (Dict (OrHeader region) html) -> Dict (OrHeader region) html
    , arrange : { header : html, region : region -> html } -> html
    }


{-| Create a custom `wrapper` to sneak functions into a
functionless `Ui`. At the `layout` phase, you decide what
your `wrapper` does:

  - `Wrapped`: Transform the rendered `html` inside the current region.
  - `Keyed`: Like Html.Keyed
  - `Nested`: Render a `Ui` with a different `html` type and then
    nest it in a supplied function.
    This transformation is applied parallelly within each region.
  - `Stateful`: Render a link or filter. The Url determines whether
    the enclosed Ui is rendered or not. You have to apply the state early.

You can either use the provided `Wrapper` or roll your own.
Advantage of rolling your own `wrapper` type: You don't need to store functions in the Ui,
which makes it comparable and serializable.

See [`Ui.Html`](Less-Ui-Html) for an example of a mostly defunctionalized wrapper.

-}
type Wrapper region narrowHtml html narrowWrapper wrapper
    = Wrapped
        { howToWrapCurrentRegion : html -> html
        , howToWrapOtherRegions : html -> html
        }
        (Ui region html wrapper)
    | Keyed { howToWrap : List ( String, html ) -> html } (List ( String, Ui region html wrapper ))
    | Nested
        { regions : List region
        , narrowLayout : CurrentLayout region narrowHtml narrowHtml narrowWrapper narrowWrapper
        , combine : { makeInnerHtml : Ui region narrowHtml narrowWrapper -> Maybe narrowHtml } -> html
        }
    | Labeled
        { label : html
        , inHeader : Bool
        }
        (Ui region html wrapper)



---- VIEW ----


{-| ⚠️ If you can, prefer one of the functions in [Less](Less) such as
[`application`](Less#application).
-}
view :
    CurrentLayout region narrowHtml_ html narrowWrapper_ wrapper
    -> Ui region html wrapper
    -> html
view layout =
    viewUi layout Header
        >> (\rendered ->
                { header =
                    Dict.get Header rendered
                        |> Maybe.toList
                        |> layout.concat
                , region =
                    \region ->
                        Dict.get (Region region) rendered
                            |> Maybe.toList
                            |> layout.concat
                }
           )
        >> layout.arrange


{-| Even though it's the same as `viewUi`, somehow Elm complains when I use `viewUi` inside `viewUi` with a different type.
I assume it's related to Elm not allowing cyclic type dependencies in `let` functions, let alone within functions.
So here is explicit polymorphism.
-}
viewOtherUi :
    CurrentLayout region narrowHtml_ html narrowWrapper_ wrapper
    -> OrHeader region
    -> Ui region html wrapper
    -> Dict (OrHeader region) html
viewOtherUi =
    viewUi


viewUi :
    CurrentLayout region narrowHtml_ html narrowWrapper_ wrapper
    -> OrHeader region
    -> Ui region html wrapper
    -> Dict (OrHeader region) html
viewUi layout region =
    let
        viewItem : Item region html wrapper -> Dict (OrHeader region) html
        viewItem item =
            case item of
                Leaf html ->
                    Dict.singleton region html

                Wrap wrapper ->
                    viewWrapper wrapper

                At innerRegion elements ->
                    viewUi layout (Region innerRegion) elements

        viewWrapper : wrapper -> Dict (OrHeader region) html
        viewWrapper wrapper =
            case layout.wrap wrapper of
                Wrapped { howToWrapCurrentRegion, howToWrapOtherRegions } elements ->
                    let
                        howToWrap : OrHeader region -> html -> html
                        howToWrap innerRegion =
                            if innerRegion == region then
                                howToWrapCurrentRegion

                            else
                                howToWrapOtherRegions
                    in
                    viewUi layout region elements
                        |> Dict.map howToWrap

                Keyed { howToWrap } keyedElements ->
                    let
                        unkeyItems : String -> Ui region html wrapper -> List (Dict (OrHeader region) html)
                        unkeyItems key =
                            List.indexedMap
                                (\i ->
                                    viewItem
                                        >> Dict.update region
                                            (Maybe.map (\html -> howToWrap [ ( key ++ String.fromInt i, html ) ]))
                                )
                    in
                    List.concatMap
                        (\( key, ui ) -> unkeyItems key ui)
                        keyedElements
                        |> layout.concatDicts

                -- map : (k -> a -> b) -> Dict k a -> Dict k b
                Nested { regions, narrowLayout, combine } ->
                    let
                        renderHtml : OrHeader region -> Dict (OrHeader region) html -> Dict (OrHeader region) html
                        renderHtml soloRegion =
                            combine
                                { makeInnerHtml =
                                    atCurrentRegion
                                        >> viewOtherUi narrowLayout soloRegion
                                        >> Dict.get soloRegion
                                }
                                |> Dict.insert soloRegion
                    in
                    Header
                        :: List.map Region regions
                        |> List.foldl renderHtml Dict.empty

                Labeled { label, inHeader } elements ->
                    let
                        labelRegion : OrHeader region
                        labelRegion =
                            if inHeader then
                                Header

                            else
                                region
                    in
                    layout.concatDicts
                        [ Dict.singleton labelRegion label
                        , viewUi layout region elements
                        ]

        atCurrentRegion : Ui region narrowHtml narrowWrapper -> Ui region narrowHtml narrowWrapper
        atCurrentRegion =
            case region of
                Header ->
                    identity

                Region r ->
                    inRegion r
    in
    List.map viewItem
        >> layout.concatDicts



---- Working with Regions ----


{-| Extend a sum type by a `Header` constructor. Every app in `Less` has a Header.
-}
type OrHeader region
    = Header
    | Region region



---- Dict Helpers ----


append : (List a -> a) -> Dict k a -> Dict k a -> Dict k a
append howToFlatten dictA dictB =
    Dict.merge
        Dict.insert
        (\k a b -> Dict.insert k (howToFlatten [ a, b ]))
        Dict.insert
        dictA
        dictB
        Dict.empty


dict :
    { concat : List (Dict k (List v)) -> Dict k (List v)
    , concatBy : (List v -> v) -> List (Dict k v) -> Dict k v
    }
dict =
    { concat = List.foldr (append List.concat) Dict.empty
    , concatBy = \howToFlatten -> List.foldr (append howToFlatten) Dict.empty
    }
