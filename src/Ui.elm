module Ui exposing
    ( Ui, Descendant
    , singleton, keyed, html, textLabel, foliage, fromList
    , wrap
    , with
    , view, toHtml
    , constant, inline, link
    , Handle
    , addLabel, addTextLabel
    , setAspect
    , repeat
    , map, indexedMap, mapList
    , map2
    , uncons
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


# Add Interactivity

@docs constant, inline, link

@docs Handle


# Convenience

@docs addLabel, addTextLabel
@docs setAspect
@docs repeat

In addition, many List functions directly work with `Ui`, for example `List.length`, `List.reverse` or `List.Extra.permutations`.
Caveats are discussed in [Advanced Usage](advanced-usage)


# Advanced Usage

Since `Ui`s are `List`s, it is easy to use the library functions from the `List` and `List.Extra` packages.
However, I recommend against it.
The big drawback when using `Ui`s as `List`s is that you cannot inspect (compare, filter, sort) them because the `Descendant` type is opaque.
It is usually easier to build exactly the `Ui` you need instead of altering and recombining them after the fact.

@docs map, indexedMap, mapList
@docs map2


## Decompose

You can directly use List decomposition functions such as `List.head`, `List.isEmpty`, `List.take n` etc. but

@docs uncons


# Conditional helpers

@docs ifJust, notIf, none

-}

import Bool.Extra as Bool
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Keyed exposing (node)
import List.Extra as List
import Maybe.Extra as Maybe
import Ui.Get as Get exposing (Get)
import Ui.Layout as Layout exposing (Layout)
import Ui.Layout.Aspect exposing (Aspect(..))
import Ui.Layout.ViewModel as ViewModel exposing (Foliage, ViewModel)
import Ui.Link as Link exposing (Link)
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
    { handle : Handle msg
    , get : Get (Ui msg)
    }



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


{-| -}
keyed : String -> Html msg -> Ui msg
keyed key =
    Tuple.pair key >> List.singleton >> foliage


{-| [Foliage](#Foliage) is a list of String-keyed Html
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



---- COMPOSE ----


{-| Nest a sub-Ui to each descendant, via a [semantic aspect](Ui.Layout.Aspect):

Note that `[] |> with ...` will always produce `[]`

    [] |> with Scene (textLabel "I want to be a scene")
        --> []

-}
with : Aspect -> Ui msg -> Ui msg -> Ui msg
with aspect subUi =
    List.map
        (\original ->
            case original of
                Twig foliage_ maybeItem ->
                    maybeItem
                        |> Maybe.unpack
                            (\() -> { get = Get.singleton aspect subUi, handle = Constant [] })
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
    (++) l >> wrap (node "label" [] >> Tuple.pair "" >> List.singleton)


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



---- MODIFY ----


{-| Shorthand for `[] |> with ...`
-}
setAspect : Aspect -> Ui msg -> Ui msg
setAspect aspect subUi =
    with aspect subUi []


{-| Nest the DOM here.
If you wrap, and then define the contextual aspect,
the wrapper will wrap all descendants that constitute this aspect.

    example : Ui msg
    example =
        []
            |> with Scene []
            |> wrap ((++) ( "message", Html.text "I am wrapped" ))
            |> with Control []

Now, let's see what happens if we define a contextual aspect.

    []
        |> with Control example

This will output:
`Scene -> []`,
`Control -> "I am wrapped" []`

-}
wrap : (Foliage msg -> Foliage msg) -> Ui msg -> Ui msg
wrap =
    Wrap >> (<<) List.singleton


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

    List.repeat 10 []
        |> indexedMap (\i _ -> textLabel (String.fromInt i))
        |> mapList (List.intersperse (textLabel "and"))

You may find it handy to use functions such as `List.Extra.setAt` to replace the n-th descendant in a List:

    import List.Extra as List

    [ textLabel "A", textLabel "oops", textLabel "C" ]
        |> mapList (List.setAt 1 (textLabel "the other B"))

or to remove a descendant:

    [ textLabel "A", textLabel "oops", textLabel "C" ]
        |> mapList (List.remove 1)

-}
mapList : (List (Ui msg) -> List (Ui msg2)) -> Ui msg -> Ui msg2
mapList fu =
    List.map List.singleton >> fu >> List.concat


{-| Modify each descendent as a separate Ui and then recombine them.
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


{-| Attempt to separate the first descendant in the Ui.
-}
uncons : Ui msg -> Maybe ( Ui msg, Ui msg )
uncons =
    List.uncons >> Maybe.map (Tuple.mapFirst List.singleton)



---- VIEW ----


{-| Generate [keyed Html (Foliage)](Ui.Layout.ViewModel#Foliage)
-}
view : Url -> Layout -> Ui msg -> Foliage msg
view url layout =
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
                ( appendHandle, itemMask ) =
                    viewHandle item.handle
            in
            item.get
                |> Get.mapByKey
                    (\key -> viewUi ( key, mask >> itemMask ))
                |> Get.values [ Scene, Control, Info ]
                |> ViewModel.concat
                |> appendHandle aspect

        viewHandle : Handle msg -> ( Aspect -> ViewModel msg -> ViewModel msg, Mask (Ui msg) )
        viewHandle h =
            case h of
                Constant html_ ->
                    ( \_ -> keyByIndex html_ |> ViewModel.appendHandle
                    , Mask.transparent
                    )

                Link aspects link_ ->
                    Link.view url link_
                        |> (\( foliage_, mask_ ) ->
                                ( \_ -> ViewModel.appendHandle [ ( Url.toString url, Html.map never (foliage_ [] []) ) ]
                                , mask_ aspects
                                )
                           )

                Inline aspects link_ ->
                    Link.view url link_
                        |> (\( foliage_, mask_ ) ->
                                ( Get.singleton
                                    >> (|>) [ ( Url.toString url, Html.map never (foliage_ [] []) ) ]
                                    >> ViewModel.appendGet
                                , mask_ aspects
                                )
                           )

                Custom c ->
                    c
    in
    viewUi ( Scene, Mask.transparent )
        >> Layout.view
        >> (|>) layout



---- Working with Handles ----


{-| **Constant:** Add your own stuff into the Handle aspect
**Link:** Add a [Link](Ui.Link#Link) into the Handle aspect
**Inline>** Add a [Link](Ui.Link#Link) into the current aspect

  - Popup -> Scene Disclosure in window - volatile - -> TODO
  - Summarize -> Scene Disclosure inline - volatile - -> summary..details [a href="{currentPath}?summarize={flag}"] <- use austinshenk/elm-w3
  - Toggle -> Control Disclosure - persistent - -> a role="switch" href="{currentPath}?toggle={flag}"
  - Static -> Noop
  - Alternate -> Link to path0 - -> a href="{path0}?rerouteLink=path1"

volatile: `Flag`s are reset when path changes
persistent: `Flag`s persist across path changes
unique: A single screen has at most one such `Flag`

-}
type Handle msg
    = Constant (List (Html msg))
    | Link (List Aspect) Link
    | Inline (List Aspect) Link
    | Custom ( Aspect -> ViewModel msg -> ViewModel msg, Mask (Ui msg) )


{-| Here you can add your own link, button, input, or indicator.
-}
constant : List (Html msg) -> Ui msg
constant =
    Constant >> fromHandle


{-| Generate a relative link. (Ui.Href#Msg)[Consuls `Ui.Href#Msg`] for details.
-}
link : List Aspect -> Link -> Ui msg
link aspects =
    Link aspects >> fromHandle


{-| Generate a relative link. (Ui.Href#Msg)[Consuls `Ui.Href#Msg`] for details.
-}
inline : List Aspect -> Link -> Ui msg
inline aspects =
    Inline aspects >> fromHandle


fromHandle : Handle msg -> Ui msg
fromHandle h =
    fromItem { handle = h, get = \_ -> Nothing }



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
    node "" []
