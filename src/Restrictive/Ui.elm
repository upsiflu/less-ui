module Restrictive.Ui exposing
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
    , custom, page, byLocation
    , indexedMap, mapList
    , mapEach, map2
    , ifJust, notIf, none
    )

{-| Separate [State](Ui.State) and [Layout](Ui.Layout) of interface elements from the main model
and build accessible patterns orthogonal to the Dom tree.

Ui is headless (like elm-widgets will be).

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
@docs custom, page, byLocation


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
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout exposing (Layout)
import Restrictive.Layout.Region as Region exposing (OrHeader(..))
import Restrictive.Mask as Mask exposing (Mask)
import Restrictive.State as State exposing (State)
import Url exposing (Url)


{-| -}
type alias Ui aspect html =
    List (Descendant aspect html)


{-| -}
type Descendant aspect html
    = Twig (List html) (Maybe (Item aspect html))
    | Wrap (List html -> List html) (Ui aspect html)


{-| TODO: update this comment

An Item logically nests `Ui`s and may dybamically react to the Url.
Only `Dynamic` aspects produce `Header` content.

For example,

  - an inline toggle link will
      - ask the Url whether the toggle is on or off
      - if off, then hide all regions `Mask.occludeOrAll occlusions`
      - place the toggle link itself `Get.append (Get.singleton inlineRegion (foliage link))`
      - activate the Header region `Get.mapKey Region.justRegion`
  - a page link will
      - ask the Url whether the path includes the page path
      - if yes, replace the item's `get` with the page `superimpose (Get.singleton inlineRegion pageUi)`
      - activate the Header region `Get.mapKey Region.justRegion`
      - place the page link itself `Get.append (Get.singleton Header (foliage link))`
  - a constant header (`handle`) will
      - activate the Header region `Get.mapKey Region.justRegion`
      - place the handle content `Get.append (Get.singleton Header (foliage handle))`

-}
type alias Item aspect html =
    { get : Get aspect (Ui aspect html)
    , mask : ( OrHeader aspect, Url ) -> Mask (OrHeader aspect) (Ui aspect html)
    }



---- CREATE ----


{-| -}
singleton : Ui aspect html
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
html : html -> Ui aspect html
html =
    List.singleton >> foliage


{-| [Foliage](Ui.Layout.ViewModel#Foliage) is a list of String-keyed Html
-}
foliage : List html -> Ui aspect html
foliage =
    Twig >> (|>) Nothing >> List.singleton


labelFromString : String -> Ui aspect ( String, Html msg )
labelFromString t =
    foliage [ ( t, Html.span [ Attr.class "text label" ] [ Html.text t ] ) ]


{-| `fromList = List.concatMap`
-}
fromList : (a -> Ui aspect html) -> List a -> Ui aspect html
fromList =
    List.concatMap


fromItem : Item aspect html -> Ui aspect html
fromItem =
    Just >> Twig [] >> List.singleton



---- MODIFY ----


{-| Shorthand for `singleton |> with ...`
-}
setAspect : aspect -> Ui aspect html -> Ui aspect html
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
the will wrap all descendants that constitute this aspect.

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
wrap : (List html -> List html) -> Ui aspect html -> Ui aspect html
wrap fu =
    Wrap fu >> List.singleton



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
with : aspect -> Ui aspect html -> Ui aspect html -> Ui aspect html
with aspect subUi =
    List.map
        (\original ->
            case original of
                Twig foliage_ maybeItem ->
                    Maybe.map
                        (\item_ -> Just { item_ | get = Get.addList aspect subUi item_.get })
                        maybeItem
                        |> Maybe.withDefaultLazy
                            (\() -> Just { get = Get.singleton aspect subUi, mask = always identity })
                        |> Twig foliage_

                Wrap fu ui ->
                    Wrap fu (with aspect subUi ui)
        )


{-| prepend a freeform label to the contextual aspect
-}
addLabel : Ui aspect ( String, Html msg ) -> Ui aspect ( String, Html msg ) -> Ui aspect ( String, Html msg )
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
map2 : (Ui aspect html -> Ui aspect2 html2 -> Ui aspect3 html3) -> Ui aspect html -> Ui aspect2 html2 -> Ui aspect3 html3
map2 fu a b =
    List.map2 fu (List.map List.singleton a) (List.map List.singleton b)
        |> List.concat


type alias KeyedUi aspect msg =
    Ui aspect ( String, Html msg )


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
indexedMap : (Int -> Ui aspect html -> Ui aspect html) -> Ui aspect html -> Ui aspect html
indexedMap fu =
    List.indexedMap (\i -> List.singleton >> fu i) >> List.concat


{-|

    repeat n =
        List.repeat n >> List.concat

-}
repeat : Int -> Ui aspect html -> Ui aspect html
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
mapList : (List (Ui aspect html) -> List (Ui aspect2 html2)) -> Ui aspect html -> Ui aspect2 html2
mapList fu =
    List.map List.singleton >> fu >> List.concat


{-| Modify each descendent as a separate Ui and then recombine them.

    aspect html   "A" ++ aspect html   "B" ++ aspect html   "C"
        |> mapEach ((++) (html ", "))
        ---> something like A, B, C

-}
mapEach : (Ui aspect html -> Ui aspect2 html2) -> Ui aspect html -> Ui aspect2 html2
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
uncons : Ui aspect html -> Maybe ( Ui aspect html, Ui aspect html )
uncons =
    List.uncons >> Maybe.map (Tuple.mapFirst List.singleton)



---- VIEW ----


{-| Generate [keyed Html (Foliage)](Ui.Layout.ViewModel#Foliage) `[(key:String, Html)]` for use with `Html.Keyed`

As the following example shows, you can substitute Html by any other type:

    import Ui.Layout.Aspect exposing (Aspect(..))
    import Ui.Layout.ViewModel exposing (Foliage)
    import Url exposing (Url)
    import Ui.Layout

    view_ : String -> Ui aspect html   -> Maybe (Foliage html)
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
view : State -> Layout aspect html -> Ui aspect html -> List html
view state layout =
    let
        viewUi : OrHeader aspect -> Ui aspect html -> Get (OrHeader aspect) (List html)
        viewUi aspect =
            Get.concatMap <|
                \descendant ->
                    case descendant of
                        Twig foliage_ maybeItem ->
                            Maybe.map (viewItem aspect) maybeItem
                                |> Maybe.withDefault Get.empty
                                |> Get.append (Get.singleton aspect foliage_)

                        Wrap wrapper ui_ ->
                            viewUi aspect ui_
                                |> Get.update aspect wrapper

        viewItem : OrHeader aspect -> Item aspect html -> Get (OrHeader aspect) (List html)
        viewItem aspect item =
            -- Calculate the mutation between the states     -> Get (OrHeader aspect) (Mutation(List html))
            State.map
                (\url_ ->
                    item.mask ( aspect, url_ ) (Get.mapKey Region.justRegion item.get)
                        -- Render logically nested Uis
                        |> Get.mapByKey viewUi
                        |> Get.values (Region.withHeader layout.regions)
                        |> Get.concat
                )
                state
                |> Get.mutation
                -- Resolve the mutation                          -> Get (OrHeader aspect) (List html)
                |> Get.map
                    (\mutation ->
                        case mutation of
                            Get.Substitution m ->
                                layout.substitute.current m.current ++ layout.substitute.previous m.previous

                            Get.Insertion a ->
                                a

                            Get.Deletion a ->
                                layout.forget a

                            Get.Protraction a ->
                                a
                    )
    in
    case List.head layout.regions of
        Just initial ->
            viewUi (Region initial) >> layout.view

        Nothing ->
            \_ -> []



---- Working with contingent transformations ----


{-| Here you can add your own button, input, or indicator in the Header region.
-}
handle : List html -> Ui aspect html
handle foliage_ =
    foliage foliage_
        |> Get.singleton Header
        |> Get.append
        |> always
        |> custom


{-| Whatever you add to this item will only be visible . Use `bounce` and `goTo` links to navigate to the corresponding path.
-}
page : State.Path -> Ui aspect html -> Ui aspect html
page path_ ui_ =
    custom <|
        \( region, url ) ->
            Get.singleton region ui_
                |> Get.append
                |> Mask.filter (\_ -> State.getPath url == path_)


{-| -}
byLocation : (( Maybe State.Path, State.Fragment ) -> Ui aspect html) -> Ui aspect html
byLocation fromPage =
    custom <|
        \( region, url ) ->
            fromPage (State.getLocation url)
                |> Get.singleton region
                |> Get.append


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

    viewWithState : {current : String, previous : String } -> Ui aspect html   -> List String
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
custom : (( OrHeader aspect, Url ) -> Mask (OrHeader aspect) (Ui aspect html)) -> Ui aspect html
custom mask =
    fromItem
        { get = Get.empty
        , mask = mask
        }



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
