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
    , handle
    , custom
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


## Add handles and other contingent transformations

For convenient functions and inspiration, check out the [Link](Ui.Link) module which uses [`Ui.custom`](#custom) to
interface with the Ui context.

@docs handle
@docs custom


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
import Html.Attributes as Attr
import Html.Keyed
import List.Extra as List
import Maybe.Extra as Maybe
import Ui.Get as Get exposing (Get)
import Ui.Layout exposing (Layout)
import Ui.Layout.Aspect as Aspect exposing (Aspect(..))
import Ui.Layout.ViewModel as ViewModel exposing (Foliage, ViewModel)
import Ui.Mask as Mask
import Ui.State exposing (State)
import Ui.Transformation as Transformation exposing (Transformation)
import Url exposing (Url)


{-| -}
type alias Ui html =
    List (Descendant html)


{-| -}
type Descendant html
    = Twig (Foliage html) (Maybe (Item html))
    | Wrap (Foliage html -> Foliage html) (Ui html)


{-| -}
type alias Item html =
    { contingentTransformation : Maybe (( Aspect, State ) -> Transformation html)
    , get : Get (Ui html)
    }



---- CREATE ----


{-| -}
singleton : Ui html
singleton =
    [ Twig [] Nothing ]


{-| -}
textLabel : String -> Ui (Html msg)
textLabel =
    addTextLabel >> (|>) []


{-| -}
html : html -> Ui html
html =
    Tuple.pair "" >> List.singleton >> foliage


{-| Preserve data in controls or [custom elements](https://guide.elm-lang.org/interop/custom_elements.html) even [when nodes before this are removed or added](https://guide.elm-lang.org/optimization/keyed.html).
-}
keyed : String -> html -> Ui html
keyed key =
    Tuple.pair key >> List.singleton >> foliage


{-| [Foliage](Ui.Layout.ViewModel#Foliage) is a list of String-keyed Html
-}
foliage : Foliage html -> Ui html
foliage =
    Twig >> (|>) Nothing >> List.singleton


labelFromString : String -> Ui (Html msg)
labelFromString t =
    foliage [ ( t, Html.span [ Attr.class "text label" ] [ Html.text t ] ) ]


{-| `fromList = List.concatMap`
-}
fromList : (a -> Ui html) -> List a -> Ui html
fromList =
    List.concatMap


fromItem : Item html -> Ui html
fromItem =
    Just >> Twig [] >> List.singleton



---- MODIFY ----


{-| Shorthand for `singleton |> with ...`
-}
setAspect : Aspect -> Ui html -> Ui html
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
wrap : (Foliage html -> Foliage html) -> Ui html -> Ui html
wrap =
    Wrap >> (<<) List.singleton



---- COMPOSE ----


{-| Nest a sub-Ui to each descendant, via a [semantic aspect](Ui.Layout.Aspect):

Note that an empty list will stay an empty list:

    import Ui.Layout.Aspect exposing (Aspect(..))

    [] |> with Scene (textLabel "I want to be a scene")
        --> []

    singleton ++ singleton
        |> with Scene (textLabel "I want to be a scene")
        -> ???

-}
with : Aspect -> Ui html -> Ui html -> Ui html
with aspect subUi =
    List.map
        (\original ->
            case original of
                Twig foliage_ maybeItem ->
                    maybeItem
                        |> Maybe.unpack
                            (\() -> { contingentTransformation = Nothing, get = Get.singleton aspect subUi })
                            (\it -> { it | get = Get.addList aspect subUi it.get })
                        |> Just
                        |> Twig foliage_

                Wrap fu ui ->
                    Wrap fu (with aspect subUi ui)
        )


{-| prepend a freeform label to the contextual aspect
-}
addLabel : Ui (Html msg) -> Ui (Html msg) -> Ui (Html msg)
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
map2 : (Ui html -> Ui html2 -> Ui html3) -> Ui html -> Ui html2 -> Ui html3
map2 fu a b =
    List.map2 fu (List.map List.singleton a) (List.map List.singleton b) |> List.concat


{-| convenience function to wrap a Ui into an unordered list and give it an id
-}
ul : String -> Ui (Html msg) -> Ui (Html msg)
ul idString =
    wrap (Html.Keyed.ul [ Attr.id idString ] >> Tuple.pair idString >> List.singleton)


{-| convenience function to wrap a Ui into an ordered list and give it an id
-}
ol : String -> Ui (Html msg) -> Ui (Html msg)
ol idString =
    wrap (Html.Keyed.ul [ Attr.id idString ] >> Tuple.pair idString >> List.singleton)


{-| convenience function to wrap a Ui into any Html node and give it an id
-}
node : String -> String -> Ui (Html msg) -> Ui (Html msg)
node nodeType idString =
    wrap (Html.Keyed.node nodeType [ Attr.id idString ] >> Tuple.pair idString >> List.singleton)


{-| prepend a text label to the contextual aspect
-}
addTextLabel : String -> Ui (Html msg) -> Ui (Html msg)
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

As the following example shows, you can substitute Html by any other type:

    import Ui.Layout.Aspect exposing (Aspect(..))
    import Url exposing (Url)
    import Ui.Layout

    dummyUrl : Maybe Url
    dummyUrl =
        Url.fromString "http://localhost/abc"

    myUi : Ui Int
    myUi =
        singleton
            |> with Scene ( keyed "Scene" 1 )
            |> with Info ( keyed "Info" 200 )
            |> with Control ( keyed "Control" 3 )
            |> with Info ( keyed "Info" 201 )

    Maybe.map
        (\url -> view
                    { current = url, previous = Nothing }
                    Ui.Layout.list
                    myUi
        )
        dummyUrl

        --> Just [ ("Scene", 1), ("Info", 200), ("Info", 201), ("Control", 3) ]

-}
view : { current : State, previous : Maybe State } -> Layout html -> Ui html -> Foliage html
view { current, previous } layout =
    let
        viewUi : Aspect -> Ui html -> ViewModel html
        viewUi aspect =
            ViewModel.concatMap <|
                \descendant ->
                    case descendant of
                        Twig foliage_ maybeItem ->
                            maybeItem
                                |> Maybe.unwrap ViewModel.empty (viewItem aspect)
                                |> ViewModel.appendGet (Get.singleton aspect foliage_)

                        Wrap wrapper descList ->
                            descList
                                |> viewUi aspect
                                |> ViewModel.mapGet (Get.update aspect wrapper)

        viewItem : Aspect -> Item html -> ViewModel html
        viewItem aspect item =
            let
                { addition, removal, unchanged } =
                    item.contingentTransformation
                        |> Maybe.withDefault (\_ -> Transformation.neutral)
                        |> (\apply ->
                                apply ( aspect, Maybe.withDefault current previous )
                                    |> Transformation.difference (apply ( aspect, current ))
                           )
            in
            item.get
                |> Mask.occludeList unchanged.occlude
                |> Get.mapByKey
                    (\aspect_ -> viewUi aspect_)
                |> Get.updateWhere (\aspect_ -> List.member aspect_ addition.occlude)
                    (ViewModel.mapGet (Get.map layout.markRemovals))
                |> Get.values Aspect.all
                |> ViewModel.concat
                |> ViewModel.append
                    { removal
                        | appendWhat =
                            layout.markRemovals removal.appendWhat
                    }
                |> ViewModel.append unchanged
                |> ViewModel.append addition
    in
    viewUi Scene >> layout.view



---- Working with contingent transformations ----


{-| Here you can add your own button, input, or indicator.
-}
handle : List html -> Ui html
handle html_ =
    (always >> custom)
        { occlude = [], appendWhere = Nothing, appendWhat = keyByIndex html_ }


{-| This interface is mostly interesting for library authors.

    import Ui.Layout.Aspect as Aspect exposing (Aspect(..))
    import Url exposing (Url)
    import Ui.Layout
    import Ui.Transformation exposing (Transformation)
    import Ui.State exposing (State)

    -- Custom handles --

    noControl : Ui ()
    noControl =
      custom
        (\(aspect, url) ->
            { occlude = [Control]
            , appendWhere = Nothing
            , appendWhat = [("Handle", ())]
            }
        )

    page : String -> Ui ()
    page route =
      custom
        (\(aspect, { path }) ->
            { occlude = if path == route then [] else Aspect.all
            , appendWhere = Nothing
            , appendWhat = [(route, ())]
            }
        )

    toggle : String -> Ui ()
    toggle flag =
      custom
        (\(aspect, { query }) ->
            { occlude = if Maybe.map (String.contains flag) query |> Maybe.withDefault False
                            then []
                            else Aspect.all
            , appendWhere = Nothing
            , appendWhat = [(flag, ())]
            }
        )

    -- Test --

    viewWithState : {current : String, previous : String } -> Ui html -> List String
    viewWithState state ui =
        case Url.fromString state.current of
            Nothing -> ["Invalid `current` Url"]
            Just justCurrent ->
                ui
                    |> view {current = justCurrent, previous = Url.fromString state.previous} Ui.Layout.list
                    |> List.map Tuple.first

    -- Scenarios --

    noControl
        |> with Scene ( keyed "Scene" () )
        |> with Info ( keyed "Info" () )
        |> with Control ( keyed "Control" () )
        |> viewWithState { current = "http://a/", previous = "http://a/" }
        -->  [ "Handle" ,"Scene", "Info" ]

    singleton
        |> with Scene ( noControl )
        |> with Info ( keyed "Info" () )
        |> with Control ( keyed "Control" () )
        |> viewWithState { current = "http://a/", previous = "http://a/" }
        -->  [ "Handle" ,"Info", "Control" ]

    singleton
        |> with Scene
            ( noControl
                |> with Control (keyed "Control" ())
            )
        |> viewWithState { current = "http://a/", previous = "http://a/" }
        -->  [ "Handle" ]

    singleton
        |> with Scene
            ( noControl
                |> with Control
                ( keyed "Control" ()
                    |> with Info ( keyed "Info" () )
                )
            )
        |> viewWithState { current = "http://a/", previous = "http://a/" }
        -->  [ "Handle" ]

    noControl
        |> with Scene
            ( page "/Cool"
                |> with Info (keyed "Cool page is open" ())
            )
        |> viewWithState { current = "http://a/", previous = "http://a/" }
        -->  [ "Handle", "/Cool" ]

    [ page "/Cool" |> with Info (keyed "Cool page is open" ())
    , page "/Hot" |> with Info (keyed "Hot page is open" ())
    ]
        |> List.concat
        |> viewWithState { current = "http://a/", previous = "http://a/" }
        -->  [ "/Cool", "/Hot" ]

    [ page "/Cool" |> with Info (keyed "Cool page is open" ())
    , page "/Hot" |> with Info (keyed "Hot page is open" ())
    ]
        |> List.concat
        |> viewWithState { current = "http://a.a/Cool", previous = "http://a/" }
        -->  [ "/Cool", "/Hot", "Cool page is open" ]

    [ page "/Cool" |> with Info (keyed "Cool page is open" ())
    , page "/Hot" |> with Info (keyed "Hot page is open" ())
    ]
        |> List.concat
        |> viewWithState { current = "http://a.a/Cool", previous = "http://a/Hot" }
        -->  [ "/Cool", "/Hot", "Cool page is open", "-" ]

-}
custom : (( Aspect, Url ) -> Transformation html) -> Ui html
custom h =
    fromItem { contingentTransformation = Just h, get = Get.empty }



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


keyByIndex : List html -> Foliage html
keyByIndex =
    List.indexedMap (String.fromInt >> Tuple.pair)


{-| If you use the default Elm Html library, this is for you
-}
toHtml : Foliage (Html msg) -> Html msg
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
