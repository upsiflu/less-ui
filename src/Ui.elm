module Ui exposing
    ( Ui, Descendant
    , singleton, html, textLabel, foliage, fromList
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
    , indexedMap, mapList
    , mapEach, map2
    , ifJust, notIf, none
    )

{-| Separate [State](Ui.State) and [Layout](Ui.Layout) of interface elements from the main model
and build accessible patterns orthogonal to the Dom tree

@docs Ui, Descendant


# Create

@docs singleton, html, textLabel, foliage, fromList


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


### Keyed Wrappers

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

@docs indexedMap, mapList
@docs mapEach, map2


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
import Ui.Layout.ViewModel as ViewModel exposing (ViewModel)
import Ui.Mask as Mask
import Ui.State exposing (State)
import Ui.Transformation as Transformation exposing (Transformation)
import Url exposing (Url)


{-| -}
type alias Ui aspect html wrapper =
    List (Descendant aspect html wrapper)


{-| -}
type Descendant aspect html wrapper
    = Twig (List html) (Maybe (Item aspect html wrapper))
    | Wrap wrapper (Ui aspect html wrapper)


{-| -}
type alias Item aspect html wrapper =
    { contingentTransformation : Maybe (( aspect, State ) -> Transformation aspect html)
    , get : Get aspect (Ui aspect html wrapper)
    }



---- CREATE ----


{-| -}
singleton : Ui aspect html wrapper
singleton =
    [ Twig [] Nothing ]


{-| -}
textLabel : String -> KeyedUi aspect msg
textLabel =
    addTextLabel >> (|>) []


{-| To preserve data in controls or [custom elements](https://guide.elm-lang.org/interop/custom_elements.html)
even [when nodes before this are removed or added](https://guide.elm-lang.org/optimization/keyed.html),
use (key, Html) and then wrap with `Html.Keyed.node`.
-}
html : html -> Ui aspect html wrapper
html =
    List.singleton >> foliage


{-| [Foliage](Ui.Layout.ViewModel#Foliage) is a list of String-keyed Html
-}
foliage : List html -> Ui aspect html wrapper
foliage =
    Twig >> (|>) Nothing >> List.singleton


labelFromString : String -> Ui aspect ( String, Html msg ) foliage
labelFromString t =
    foliage [ ( t, Html.span [ Attr.class "text label" ] [ Html.text t ] ) ]


{-| `fromList = List.concatMap`
-}
fromList : (a -> Ui aspect html wrapper) -> List a -> Ui aspect html wrapper
fromList =
    List.concatMap


fromItem : Item aspect html wrapper -> Ui aspect html wrapper
fromItem =
    Just >> Twig [] >> List.singleton



---- MODIFY ----


{-| Shorthand for `singleton |> with ...`
-}
setAspect : aspect -> Ui aspect html wrapper -> Ui aspect html wrapper
setAspect aspect subUi =
    with aspect subUi singleton


{-| Nest Foliage at the given `Aspect`.

    a = ──◉◉●───◆◆◉──
        |> wrap ▒

        -> ▒──◉◉●───◆◆◉──

    singleton
        |> with  ◉  ▒──◉◉●───◆◆◉──

        ->
                       ╭▒▒╮       ╭▒╮
                     ──┘◉◉└●────◆◆┘◉└──

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
wrap : wrapper -> Ui aspect html wrapper -> Ui aspect html wrapper
wrap wrapper =
    Wrap wrapper >> List.singleton



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
with : aspect -> Ui aspect html wrapper -> Ui aspect html wrapper -> Ui aspect html wrapper
with aspect subUi =
    List.map
        (\original ->
            case original of
                Twig foliage_ maybeItem ->
                    Maybe.map
                        (\it -> Just { it | get = Get.addList aspect subUi it.get })
                        maybeItem
                        |> Maybe.withDefaultLazy
                            (\() -> Just { contingentTransformation = Nothing, get = Get.singleton aspect subUi })
                        |> Twig foliage_

                Wrap fu ui ->
                    Wrap fu (with aspect subUi ui)
        )


{-| prepend a freeform label to the contextual aspect
-}
addLabel : Ui aspect ( String, Html msg ) (List ( String, Html a ) -> List ( String, Html a )) -> Ui aspect ( String, Html msg ) (List ( String, Html a ) -> List ( String, Html a )) -> Ui aspect ( String, Html msg ) (List ( String, Html a ) -> List ( String, Html a ))
addLabel label_ ui =
    wrap
        (Html.Keyed.node "label" []
            >> Tuple.pair ""
            >> List.singleton
        )
        (label_ ++ ui)


{-| Combine descendents from two `Ui`s. If one `Ui` is longer, its excessive elements are dropped.

You can easily implement higher order `mapN`s:

    map3 fu a b c =
        List.map3 fu
            (List.map List.singleton a)
            (List.map List.singleton b)
            (List.map List.singleton c)
            |> List.concat

-}
map2 : (Ui aspect html wrapper -> Ui aspect2 html2 wrapper2 -> Ui aspect3 html3 wrapper3) -> Ui aspect html wrapper -> Ui aspect2 html2 wrapper2 -> Ui aspect3 html3 wrapper3
map2 fu a b =
    List.map2 fu (List.map List.singleton a) (List.map List.singleton b)
        |> List.concat


type alias KeyedUi aspect msg =
    Ui aspect ( String, Html msg ) (List ( String, Html msg ) -> List ( String, Html msg ))


{-| convenience function to wrap a Ui into an unordered list and give it an id
-}
ul : String -> KeyedUi aspect msg -> KeyedUi aspect msg
ul idString =
    wrap (Html.Keyed.ul [ Attr.id idString ] >> Tuple.pair idString >> List.singleton)


{-| convenience function to wrap a Ui into an ordered list and give it an id
-}
ol : String -> KeyedUi aspect msg -> KeyedUi aspect msg
ol idString =
    wrap (Html.Keyed.ul [ Attr.id idString ] >> Tuple.pair idString >> List.singleton)


{-| convenience function to wrap a Ui into any Html node and give it an id
-}
node : String -> String -> KeyedUi aspect msg -> KeyedUi aspect msg
node nodeType idString =
    wrap (Html.Keyed.node nodeType [ Attr.id idString ] >> Tuple.pair idString >> List.singleton)


{-| prepend a text label to the contextual aspect
-}
addTextLabel : String -> KeyedUi aspect msg -> KeyedUi aspect msg
addTextLabel =
    labelFromString >> addLabel



-- Functions inherited from List --


{-| Modify descendent `Ui`s according to their order.

    indexedMap (\i -> addTextLabel (String.fromInt i)) (textLabel "I am a labeled label")

-}
indexedMap : (Int -> Ui aspect html wrapper -> Ui aspect html wrapper) -> Ui aspect html wrapper -> Ui aspect html wrapper
indexedMap fu =
    List.indexedMap (\i -> List.singleton >> fu i) >> List.concat


{-|

    repeat n =
        List.repeat n >> List.concat

-}
repeat : Int -> Ui aspect html wrapper -> Ui aspect html wrapper
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
mapList : (List (Ui aspect html wrapper) -> List (Ui aspect2 html2 wrapper2)) -> Ui aspect html wrapper -> Ui aspect2 html2 wrapper2
mapList fu =
    List.map List.singleton >> fu >> List.concat


{-| Modify each descendent as a separate Ui and then recombine them.

    aspect html wrapper "A" ++ aspect html wrapper "B" ++ aspect html wrapper "C"
        |> mapEach ((++) (html ", "))
        ---> something like A, B, C

-}
mapEach : (Ui aspect html wrapper -> Ui aspect2 html2 wrapper2) -> Ui aspect html wrapper -> Ui aspect2 html2 wrapper2
mapEach fu =
    List.concatMap (List.singleton >> fu)



---- DECOMPOSE ----


{-| Attempt to separate the first descendant in the Ui

    import Ui.Layout.ViewModel exposing (Foliage)
    import Ui.Layout
    import Url

    view_ :  (Ui html, Ui html) -> Foliage html
    view_ =
        Url.fromString "http://a/"
            |> Maybe.map
                (\url ->
                    Tuple.first >> view { current = url, previous = Nothing } Ui.Layout.list
                )
            |> Maybe.withDefault (\_-> [])

    keyed "1" () ++ keyed "2" () ++ keyed "3" ()
        |> uncons
        |> Maybe.map view_
        --> Just [ ("1", ())]

    singleton
        |> uncons
        |> Maybe.map view_
        --> Just []

    []
        |> uncons
        |> Maybe.map view_
        --> Nothing

-}
uncons : Ui aspect html wrapper -> Maybe ( Ui aspect html wrapper, Ui aspect html wrapper )
uncons =
    List.uncons >> Maybe.map (Tuple.mapFirst List.singleton)



---- VIEW ----


{-| Generate [keyed Html (Foliage)](Ui.Layout.ViewModel#Foliage) `[(key:String, Html)]` for use with `Html.Keyed`

As the following example shows, you can substitute Html by any other type:

    import Ui.Layout.Aspect exposing (Aspect(..))
    import Ui.Layout.ViewModel exposing (Foliage)
    import Url exposing (Url)
    import Ui.Layout

    view_ : String -> Ui aspect html wrapper -> Maybe (Foliage html)
    view_ urlString ui_=
        Url.fromString urlString
            |> Maybe.map
                (\url -> view { current = url, previous = Nothing } Ui.Layout.list ui_)


    myUi : Ui Int
    myUi =
        singleton
            |> with Scene ( keyed "Scene" 1 )
            |> with Info ( keyed "Info" 200 )
            |> with Control ( keyed "Control" 3 )
            |> with Info ( keyed "Info" 201 )

    view_ "http://a/" myUi
        --> Just [ ("Scene", 1), ("Info", 200), ("Info", 201), ("Control", 3) ]

-}
view : { current : State, previous : Maybe State } -> Layout aspect html wrapper -> Ui aspect html wrapper -> List html
view { current, previous } layout =
    let
        viewUi : aspect -> Ui aspect html wrapper -> ViewModel (Maybe aspect) html
        viewUi aspect =
            ViewModel.concatMap <|
                \descendant ->
                    case descendant of
                        Twig foliage_ maybeItem ->
                            Maybe.map (viewItem aspect) maybeItem
                                |> Maybe.withDefault ViewModel.empty
                                |> ViewModel.appendGet (Get.singleton (Just aspect) foliage_)

                        Wrap wrapper descList ->
                            viewUi aspect descList
                                |> ViewModel.maskGet (Get.update (Just aspect) (layout.wrap wrapper))

        viewItem : aspect -> Item aspect html wrapper -> ViewModel (Maybe aspect) html
        viewItem aspect item =
            let
                { addition, removal, unchanged } =
                    Maybe.withDefault (\_ -> Transformation.neutral) item.contingentTransformation
                        |> (\apply ->
                                Transformation.difference
                                    (apply ( aspect, current ))
                                    (apply ( aspect, Maybe.withDefault current previous ))
                           )

                -- calculate
                --
            in
            Mask.occludeList unchanged.occlude item.get
                -- DRAW ADDED AND REMOVED VIEWMODELS
                |> Get.mapByKey
                    (\aspect_ -> viewUi aspect_)
                -- INCLUDE THE NOTHING ASPECT
                |> Get.mapKey identity
                -- MARK NEWLY OCCLUDED VIEWMODELS
                |> Get.updateWhere
                    (\key -> List.member key (List.map Just addition.occlude))
                    (layout.wrap layout.forget
                        |> Get.map
                        |> ViewModel.maskGet
                    )
                ------------
                -- DONE WITH GET -- FLATTEN THE HIERARCHY -- IT'S OVER!
                |> Get.values (Nothing :: List.map Just layout.all)
                |> ViewModel.concat
                ------------
                -- NOW FOR DYNAMIC APPENDING
                |> ViewModel.append unchanged
                |> ViewModel.append addition
                -- REMOVAL-APPEND MUST BE MARKED AS NEWLY REMOVED BY LAYOUT.
                --
                -- WE HAVE
                -- A mask for removals that responds to any aspect, and is able to map aspects --
                -- Do we really want to move things around on the screen?
                -- I.e. if I go away from a page, or if I close a dialog, should this dialog appear somewhere,
                -- in a different form, to remind me what was there?
                -- Probably not. Probably we just want to replace the things with a fading memory, just to keep the
                -- transitions through the app a little less jarring...
                -- WE WANT
                |> ViewModel.append
                    { removal
                        | appendWhat = layout.wrap layout.forget removal.appendWhat
                    }
    in
    case List.head layout.all of
        Just initial ->
            viewUi initial >> layout.view

        Nothing ->
            \_ -> []



---- Working with contingent transformations ----


{-| Here you can add your own button, input, or indicator.
-}
handle : List html -> Ui aspect html wrapper
handle html_ =
    (always >> custom)
        { occlude = [], appendWhere = Nothing, appendWhat = html_ }


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

    viewWithState : {current : String, previous : String } -> Ui aspect html wrapper -> List String
    viewWithState state ui =
        case Url.fromString state.current of
            Nothing ->
                ["Invalid `current` Url: "++state.current]
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
custom : (( aspect, Url ) -> Transformation aspect html) -> Ui aspect html wrapper
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


{-| If you use the default Elm Html library, this is for you
-}
toHtml : List ( String, Html msg ) -> Html msg
toHtml =
    Html.Keyed.node "div" []



---- Testing ----
{- Get aspect the length of keyed Html for each Aspect, given the Url "<http://localhost/">

   lengths : Ui msg -> Get aspect Int
   lengths ui =
       Url.fromString "http://localhost/"
           |> Maybe.map (\url -> render url ui |> .get |> Get.map List.length)
           |> Maybe.withDefault Get.empty
-}
