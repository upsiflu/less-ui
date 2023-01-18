module Ui exposing
    ( Ui, Descendant
    , singleton, keyed, html, textLabel, foliage, fromList
    , wrap
    , with
    , view, toHtml
    , addLabel, addTextLabel
    , setAspect
    , repeat
    , ol, ul, node
    , uncons
    , constant
    , custom, Custom(..)
    , map, indexedMap, mapList
    , map2
    , ifJust, notIf, none
    )

{-| Separate [State](Ui.State) and [Layout](Ui.Layout) of interface elements from the main model
and build accessible patterns orthogonal to the Dom tree

@docs Ui, Descendant


# Create

@docs singleton, keyed, html, textLabel, foliage, fromList


# Modify

@docs wrap


# Compose

@docs with

To merge two `Ui`s on the same level, use [`++`](https://package.elm-lang.org/packages/elm/core/latest/Basics#++) as in `ui1 ++ ui2`


# View

@docs view, toHtml

---


# Convenience

@docs addLabel, addTextLabel
@docs setAspect
@docs repeat

In addition, many List functions directly work with `Ui`, for example `List.length`, `List.reverse` or `List.Extra.permutations`.
Caveats are discussed in [Advanced Usage](advanced-usage)


### Wrap

@docs ol, ul, node


### Decompose

@docs uncons


# Advanced Usage


## Add Handles

For convenient functions, check out the [Link](Ui.Link) module.

@docs constant
@docs custom, Custom


## Map

Since `Ui`s are `List`s, it is easy to use the library functions from the `List` and `List.Extra` packages.
However, I recommend against it.

The big drawback when using `Ui`s as `List`s is that you cannot inspect (compare, filter, sort) them because the `Descendant` type is opaque.
It is usually easier to build exactly the `Ui` you need instead of altering and recombining them after the fact.

@docs map, indexedMap, mapList
@docs map2


# Conditional helpers

@docs ifJust, notIf, none

-}

import Bool.Extra as Bool
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Keyed
import List.Extra as List
import Maybe.Extra as Maybe
import Ui.Get as Get exposing (Get)
import Ui.Layout as Layout exposing (Layout)
import Ui.Layout.Aspect exposing (Aspect(..))
import Ui.Layout.ViewModel as ViewModel exposing (Foliage, ViewModel)
import Ui.Mask as Mask exposing (Mask)
import Url exposing (Url)


{-| -}
type alias Ui msg =
    List (Descendant msg)


{-| -}
type Descendant msg
    = Twig (Foliage msg) (Maybe (Item msg))
    | Wrap (Foliage msg -> Foliage msg) (Ui msg)


{-| -}
type alias Item msg =
    { dynamic : Handle msg
    , get : Get (Ui msg)
    }


{-| -}
type alias Handle msg =
    ( Aspect, Url ) -> List (Custom msg)



---- CREATE ----


{-| -}
singleton : Ui msg
singleton =
    [ Twig [] Nothing ]


{-| -}
textLabel : String -> Ui msg
textLabel =
    addTextLabel >> (|>) []


{-| -}
html : Html msg -> Ui msg
html =
    Tuple.pair "" >> List.singleton >> foliage


{-| Preserve data in controls or [custom elements](https://guide.elm-lang.org/interop/custom_elements.html) even [when nodes before this are removed or added](https://guide.elm-lang.org/optimization/keyed.html).
-}
keyed : String -> Html msg -> Ui msg
keyed key =
    Tuple.pair key >> List.singleton >> foliage


{-| [Foliage](Ui.Layout.ViewModel#Foliage) is a list of String-keyed Html
-}
foliage : Foliage msg -> Ui msg
foliage =
    Twig >> (|>) Nothing >> List.singleton


labelFromString : String -> Ui msg
labelFromString t =
    foliage [ ( t, Html.span [ class "text label" ] [ Html.text t ] ) ]


{-| `fromList = List.concatMap`
-}
fromList : (a -> Ui msg) -> List a -> Ui msg
fromList =
    List.concatMap


fromItem : Item msg -> Ui msg
fromItem =
    Just >> Twig [] >> List.singleton



---- MODIFY ----


{-| Shorthand for `singleton |> with ...`
-}
setAspect : Aspect -> Ui msg -> Ui msg
setAspect aspect subUi =
    with aspect subUi singleton


{-| Nest the DOM here.
If you wrap, and then define the contextual aspect,
the wrapper will wrap all descendants that constitute this aspect.

    example : Ui msg
    example =
        singleton
            |> with Scene []
            |> wrap ((++) ( "message", Html.text "I am wrapped" ))
            |> with Control []

Now, let's see what happens if we define a contextual aspect.

    singleton
        |> with Info example

This will output:
`Scene -> []`,
`Control -> []`,
`Info -> "I am wrapped" []`

-}
wrap : (Foliage msg -> Foliage msg) -> Ui msg -> Ui msg
wrap =
    Wrap >> (<<) List.singleton



---- COMPOSE ----


{-| Nest a sub-Ui to each descendant, via a [semantic aspect](Ui.Layout.Aspect):

Note that an empty list will stay an empty list:

    [] |> with Scene (textLabel "I want to be a scene")
        --> []

    singleton ++ singleton
        |> with Scene (textLabel "I want to be a scene")
        --> ???

-}
with : Aspect -> Ui msg -> Ui msg -> Ui msg
with aspect subUi =
    List.map
        (\original ->
            case original of
                Twig foliage_ maybeItem ->
                    maybeItem
                        |> Maybe.unpack
                            (\() -> { dynamic = \_ -> [], get = Get.singleton aspect subUi })
                            (\it -> { it | get = Get.addList aspect subUi it.get })
                        |> Just
                        |> Twig foliage_

                Wrap fu ui ->
                    Wrap fu (with aspect subUi ui)
        )


{-| prepend a freeform label to the contextual aspect
-}
addLabel : Ui msg -> Ui msg -> Ui msg
addLabel l =
    (++) l >> wrap (Html.Keyed.node "label" [] >> Tuple.pair "" >> List.singleton)


{-| Combine descendents from two `Ui`s. If one `Ui` is longer, its excessive elements are dropped.

You can easily implement higher order `mapN`s:

    map3 fu a b c =
        List.map3 fu
            (List.map List.singleton a)
            (List.map List.singleton b)
            (List.map List.singleton c)
            |> List.concat

-}
map2 : (Ui msg -> Ui msg2 -> Ui msg3) -> Ui msg -> Ui msg2 -> Ui msg3
map2 fu a b =
    List.map2 fu (List.map List.singleton a) (List.map List.singleton b) |> List.concat


{-| convenience function to wrap a Ui into an unordered list and give it an id
-}
ul : String -> Ui msg -> Ui msg
ul idString =
    wrap (Html.Keyed.ul [ id idString ] >> Tuple.pair idString >> List.singleton)


{-| convenience function to wrap a Ui into an ordered list and give it an id
-}
ol : String -> Ui msg -> Ui msg
ol idString =
    wrap (Html.Keyed.ul [ id idString ] >> Tuple.pair idString >> List.singleton)


{-| convenience function to wrap a Ui into any Html node and give it an id
-}
node : String -> String -> Ui msg -> Ui msg
node nodeType idString =
    wrap (Html.Keyed.node nodeType [ id idString ] >> Tuple.pair idString >> List.singleton)


{-| prepend a text label to the contextual aspect
-}
addTextLabel : String -> Ui msg -> Ui msg
addTextLabel =
    labelFromString >> addLabel



-- Functions inherited from List --


{-| Modify descendent `Ui`s according to their order.

    indexedMap (\i -> addTextLabel (String.fromInt i)) (textLabel "I am a labeled label")

-}
indexedMap : (Int -> Ui msg -> Ui msg2) -> Ui msg -> Ui msg2
indexedMap fu =
    List.indexedMap (\i -> List.singleton >> fu i) >> List.concat


{-|

    repeat n =
        List.repeat n >> List.concat

-}
repeat : Int -> Ui msg -> Ui msg
repeat n =
    List.repeat n >> List.concat


{-|

    List.repeat 10 ()
        |> indexedMap (\i _ -> textLabel (String.fromInt i))
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
mapList : (List (Ui msg) -> List (Ui msg2)) -> Ui msg -> Ui msg2
mapList fu =
    List.map List.singleton >> fu >> List.concat


{-| Modify each descendent as a separate Ui and then recombine them.

    textLabel "A" ++ textLabel "B" ++ textLabel "C"
        |> map ((++) textLabel ", ")
        ---> something like A, B, C

-}
map : (Ui msg -> Ui msg2) -> Ui msg -> Ui msg2
map fu =
    List.concatMap (List.singleton >> fu)



{-
   mapMsg : (msg -> msg2) -> Ui msg -> Ui msg2
   mapMsg fu =
       let
           mapHandleMsg : Handle msg -> Handle msg2
           mapHandleMsg handle =
               case handle of
                   Constant (List (Html msg))
                   Toggle Flag (List (Html Never))
                   Summarize Flag (Bool -> List (Html Never))
                   Alternate ( Path, Path ) (List (Html Never))



           mapItemMsg : Item msg -> Item msg2
           mapItemMsg ({ handle, get }) =
               { handle = mapHandleMsg handle
               , get = get>>mapMsg fu
               }

           mapDescMsg : Descendant msg -> Descendant msg2
           mapDescMsg desc =
               case desc of
                   Leaf (foliage) (maybeItem) ->
                       Leaf
                           (List.map (Tuple.mapSecond (Html.map fu)) foliage)
                           (Maybe.map (mapItemMsg))
                   Wrap (foliageFu) (ui) ->
                       Wrap identity (mapMsg fu ui)
       in
       List.map mapDescMsg
-}
---- DECOMPOSE ----


{-| Attempt to separate the first descendant in the Ui
-}
uncons : Ui msg -> Maybe ( Ui msg, Ui msg )
uncons =
    List.uncons >> Maybe.map (Tuple.mapFirst List.singleton)



---- VIEW ----


{-| Generate [keyed Html (Foliage)](Ui.Layout.ViewModel#Foliage) `[(key:String, Html)]` for use with `Html.Keyed`
-}
view : Url -> Layout -> Ui msg -> Foliage msg
view url layout =
    render url
        >> Layout.view
        >> (|>) layout


render : Url -> Ui msg -> ViewModel msg
render url =
    let
        viewUi : ( Aspect, Mask (Ui msg) ) -> Ui msg -> ViewModel msg
        viewUi ( aspect, mask ) =
            ViewModel.concatMap <|
                \descendant ->
                    case descendant of
                        Twig foliage_ maybeItem ->
                            maybeItem
                                |> Maybe.unwrap ViewModel.empty (viewItem ( aspect, mask ))
                                |> ViewModel.appendGet (Get.singleton aspect foliage_)

                        Wrap wrapper descList ->
                            descList
                                |> viewUi ( aspect, mask )
                                |> ViewModel.mapGet (Get.update aspect wrapper)

        viewItem : ( Aspect, Mask (Ui msg) ) -> Item msg -> ViewModel msg
        viewItem ( aspect, mask ) item =
            let
                ( transform, mask_ ) =
                    item.dynamic ( aspect, url )
                        |> List.foldr
                            (\custom_ ( t, m ) ->
                                case custom_ of
                                    TransformViewModel t_ ->
                                        ( t >> t_, m )

                                    MaskDescendents m_ ->
                                        ( t, m >> m_ )
                            )
                            ( identity, mask )
            in
            item.get
                |> Get.mapByKey
                    (\aspect_ -> viewUi ( aspect_, mask_ ))
                |> Get.values [ Scene, Control, Info ]
                |> ViewModel.concat
                |> transform
    in
    viewUi ( Scene, Mask.transparent )



---- Working with Handles ----


{-| Build your own transformation! Useful for links etc.
-}
type Custom msg
    = TransformViewModel (ViewModel msg -> ViewModel msg)
    | MaskDescendents (Mask (Ui msg))


{-| Here you can add your own button, input, or indicator.
-}
constant : List (Html msg) -> Ui msg
constant html_ =
    (always >> custom)
        [ TransformViewModel (ViewModel.appendHandle (keyByIndex html_)) ]


{-| This interface is mostly interesting for library authors.
-}
custom : (( Aspect, Url ) -> List (Custom msg)) -> Ui msg
custom h =
    fromItem { dynamic = h, get = \_ -> Nothing }



---- Conditional Views ----


{-| -}
ifJust : (a -> Html msg) -> Maybe a -> Html msg
ifJust fu =
    Maybe.map fu >> Maybe.withDefault (Html.text "")


{-| -}
notIf : Bool -> Html msg -> Html msg
notIf =
    Bool.ifElse
        (\_ -> Html.text "")
        identity


{-| -}
none : Html msg
none =
    Html.text ""


keyByIndex : List (Html msg) -> Foliage msg
keyByIndex =
    List.indexedMap (String.fromInt >> Tuple.pair)


{-| If you use the default Elm Html library, this is for you
-}
toHtml : Foliage msg -> Html msg
toHtml =
    Html.Keyed.node "div" []



---- Testing ----
{- Get the length of keyed Html for each Aspect, given the Url "<http://localhost/">

   lengths : Ui msg -> Get Int
   lengths ui =
       Url.fromString "http://localhost/"
           |> Maybe.map (\url -> render url ui |> .get |> Get.map List.length)
           |> Maybe.withDefault Get.empty
-}
