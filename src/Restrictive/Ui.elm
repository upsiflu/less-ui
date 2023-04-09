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
    , indexedMapList, mapList
    , mapEach
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

    {-| Combines descendents from two `Ui`s. If one `Ui` is longer, its excessive elements are dropped.
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
the `Descendant` type is opaque.
It is usually easier to build exactly the `Ui` you need instead of altering and recombining them after the fact.

@docs indexedMapList, mapList
@docs mapEach


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
type alias Ui region wrapper html =
    List (Descendant region wrapper html)


{-| -}
type Descendant region wrapper html
    = Twig (List html) (Maybe (Item region wrapper html))
    | Wrap wrapper (Ui region wrapper html)


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
type alias Item region wrapper html =
    { get : Get region (Ui region wrapper html)
    , mask : ( OrHeader region, Url ) -> Mask (OrHeader region) (Ui region wrapper html)
    }



---- CREATE ----


{-| -}
singleton : Ui region wrapper html
singleton =
    [ Twig [] Nothing ]


{-| -}
textLabel : String -> KeyedUi region msg
textLabel =
    addTextLabel >> (|>) []


{-| To preserve data in controls or [custom elements](https://guide.elm-lang.org/interop/custom_elements.html)
even [when nodes before this are removed or added](https://guide.elm-lang.org/optimization/keyed.html),
use (key, Html) and then wrap with `Html.Keyed.node`.
-}
html : html -> Ui region wrapper html
html =
    List.singleton >> foliage


{-| [Foliage](Ui.Layout.ViewModel#Foliage) is a list of String-keyed Html
-}
foliage : List html -> Ui region wrapper html
foliage =
    Twig >> (|>) Nothing >> List.singleton


labelFromString : String -> Ui region wrapper ( String, Html msg )
labelFromString t =
    foliage [ ( t, Html.span [ Attr.class "text label" ] [ Html.text t ] ) ]


{-| `fromList = List.concatMap`
-}
fromList : (a -> Ui region wrapper html) -> List a -> Ui region wrapper html
fromList =
    List.concatMap


fromItem : Item region wrapper html -> Ui region wrapper html
fromItem =
    Just >> Twig [] >> List.singleton



---- MODIFY ----


{-| Shorthand for `singleton |> with ...`
-}
setAspect : region -> Ui region wrapper html -> Ui region wrapper html
setAspect region subUi =
    with region subUi singleton


{-| Nest Foliage at the given `Aspect`.

    a = ──◉◉●───◆◆◉──
        |> wrap ▒

        -> ▒──◉◉●───◆◆◉──

    singleton
        |> with  ◉  ▒──◉◉●───◆◆◉──

        ->
                       ╭▒▒╮       ╭▒╮
                     ──┘◉◉└●────◆◆┘◉└──

If you wrap, and then define the contextual region,
the will wrap all descendants that constitute this region.

    example : Ui msg
    example =
        singleton
            |> with Scene []
            |> wrap ((++) ( "message", Html.text "I am wrapped" ))
            |> with Control []

Now, let's see what happens if we define a contextual region.

    singleton
        |> with Info example

This will output:
`Scene -> []`,
`Control -> []`,
`Info -> "I am wrapped" []`

-}
wrap : wrapper -> Ui region wrapper html -> Ui region wrapper html
wrap wrapper =
    Wrap wrapper >> List.singleton



---- COMPOSE ----


{-| Nest a sub-Ui to each descendant, via a [semantic region](Ui.Layout.Aspect):

Note that an empty list will stay an empty list:

    import Ui.Layout.Aspect exposing (Aspect(..))

    [] |> with Scene (textLabel "I want to be a scene")
        --> []

    singleton ++ singleton
        |> with Scene (textLabel "I want to be a scene")
        -> ???

-}
with : region -> Ui region wrapper html -> Ui region wrapper html -> Ui region wrapper html
with region subUi =
    List.map
        (\original ->
            case original of
                Twig foliage_ maybeItem ->
                    Maybe.map
                        (\item_ -> Just { item_ | get = Get.addList region subUi item_.get })
                        maybeItem
                        |> Maybe.withDefaultLazy
                            (\() -> Just { get = Get.singleton region subUi, mask = always identity })
                        |> Twig foliage_

                Wrap wrapper ui ->
                    Wrap wrapper (with region subUi ui)
        )


{-| prepend a freeform label to the contextual region
-}
addLabel : KeyedUi region msg -> Ui region (List ( String, Html msg ) -> List ( String, Html msg )) ( String, Html msg ) -> Ui region (List ( String, Html msg ) -> List ( String, Html msg )) ( String, Html msg )
addLabel label_ ui =
    wrap
        (Html.Keyed.node "label" []
            >> Tuple.pair ""
            >> List.singleton
        )
        (label_ ++ ui)


type alias KeyedUi region msg =
    Ui region (List ( String, Html msg ) -> List ( String, Html msg )) ( String, Html msg )


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


{-| prepend a text label to the contextual region
-}
addTextLabel : String -> KeyedUi region msg -> KeyedUi region msg
addTextLabel =
    labelFromString >> addLabel



-- Functions inherited from List --


{-| Modify descendent `Ui`s according to their order.

    indexedMapList (\i -> addTextLabel (String.fromInt i)) (textLabel "I am a labeled label")

-}
indexedMapList : (Int -> Ui region wrapper html -> Ui region wrapper html) -> Ui region wrapper html -> Ui region wrapper html
indexedMapList fu =
    List.indexedMap (\i -> List.singleton >> fu i) >> List.concat


{-|

    repeat n =
        List.repeat n >> List.concat

-}
repeat : Int -> Ui region wrapper html -> Ui region wrapper html
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
mapList : (List (Ui region wrapper html) -> List (Ui region2 wrapper2 html2)) -> Ui region wrapper html -> Ui region2 wrapper2 html2
mapList fu =
    List.map List.singleton >> fu >> List.concat


{-| Modify each descendent as a separate Ui and then recombine them.

    region wrapper html   "A" ++ region wrapper html   "B" ++ region wrapper html   "C"
        |> mapEach ((++) (html ", "))
        ---> something like A, B, C

-}
mapEach : (Ui region wrapper html -> Ui region2 wrapper2 html2) -> Ui region wrapper html -> Ui region2 wrapper2 html2
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
uncons : Ui region wrapper html -> Maybe ( Ui region wrapper html, Ui region wrapper html )
uncons =
    List.uncons >> Maybe.map (Tuple.mapFirst List.singleton)



---- VIEW ----


{-| Generate [keyed Html (Foliage)](Ui.Layout.ViewModel#Foliage) `[(key:String, Html)]` for use with `Html.Keyed`

As the following example shows, you can substitute Html by any other type:

    import Ui.Layout.Aspect exposing (Aspect(..))
    import Ui.Layout.ViewModel exposing (Foliage)
    import Url exposing (Url)
    import Ui.Layout

    view_ : String -> Ui region wrapper html   -> Maybe (Foliage html)
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
view : State -> Layout region wrapper html -> Ui region wrapper html -> List html
view state layout =
    let
        viewUi : OrHeader region -> Ui region wrapper html -> Get (OrHeader region) (List html)
        viewUi region =
            Get.concatMap <|
                \descendant ->
                    case descendant of
                        Twig foliage_ maybeItem ->
                            Maybe.map (viewItem region) maybeItem
                                |> Maybe.withDefault Get.empty
                                |> Get.append (Get.singleton region foliage_)

                        Wrap wrapper ui_ ->
                            viewUi region ui_
                                |> Get.update region (layout.wrap wrapper)

        viewItem : OrHeader region -> Item region wrapper html -> Get (OrHeader region) (List html)
        viewItem region item =
            -- Calculate the mutation between the states     -> Get (OrHeader region) (Mutation(List html))
            State.map
                (\url_ ->
                    item.mask ( region, url_ ) (Get.mapKey Region.justRegion item.get)
                        -- Render logically nested Uis
                        |> Get.mapByKey viewUi
                        |> Get.values (Region.withHeader layout.regions)
                        |> Get.concat
                )
                state
                |> Get.mutation
                -- Resolve the mutation                          -> Get (OrHeader region) (List html)
                |> Get.map
                    (\mutation ->
                        case mutation of
                            Get.Substitution m ->
                                layout.wrap layout.substitute.current m.current ++ layout.wrap layout.substitute.previous m.previous

                            Get.Insertion a ->
                                a

                            Get.Deletion a ->
                                layout.wrap layout.forget a

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
handle : List html -> Ui region wrapper html
handle foliage_ =
    foliage foliage_
        |> Get.singleton Header
        |> Get.append
        |> always
        |> custom


{-| Whatever you add to this item will only be visible . Use `bounce` and `goTo` links to navigate to the corresponding path.
-}
page : State.Path -> Ui region wrapper html -> Ui region wrapper html
page path_ ui_ =
    custom <|
        \( region, url ) ->
            Get.singleton region ui_
                |> Get.append
                |> Mask.filter (\_ -> State.getPath url == path_)


{-| -}
byLocation : (( Maybe State.Path, State.Fragment ) -> Ui region wrapper html) -> Ui region wrapper html
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
        (\(region, url) ->
            { occlude = [Control]
            , appendWhere = Nothing
            , appendWhat = [("Handle", ())]
            }
        )

    page : String -> Ui ()
    page route =
      custom
        (\(region, { path }) ->
            { occlude = if path == route then [] else Aspect.all
            , appendWhere = Nothing
            , appendWhat = [(route, ())]
            }
        )

    toggle : String -> Ui ()
    toggle flag =
      custom
        (\(region, { query }) ->
            { occlude = if Maybe.map (String.contains flag) query |> Maybe.withDefault False
                            then []
                            else Aspect.all
            , appendWhere = Nothing
            , appendWhat = [(flag, ())]
            }
        )

    -- Test --

    viewWithState : {current : String, previous : String } -> Ui region wrapper html   -> List String
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
custom : (( OrHeader region, Url ) -> Mask (OrHeader region) (Ui region wrapper html)) -> Ui region wrapper html
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
