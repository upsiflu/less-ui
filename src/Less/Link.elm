module Less.Link exposing
    ( Link(..)
    , fromUrl
    , toHref, apply
    , getStateSearchTerms
    , Mutation(..), mutationFromTwoStates
    , Msg(..)
    , State
    , Location, parseLocation
    , Flag, Category, SearchTerm
    )

{-|

@docs Link


## Create

@docs fromUrl


## Deconstruct

@docs toHref, apply
@docs getStateSearchTerms


## Mutations

@docs Mutation, mutationFromTwoStates


## Msg

@docs Msg


# State

@docs State

             ╔═══════════════════════Location═══════════════════╗
     ╔══Path═╩╗                                                ╔╩Fragment═╗
    /over/there?animalName=violetFerret&bigLeg&cat0=cat10=val2#rightHindLeg
                ╠Category╝→╚SearchTerm╣ ║    ║ ╠C.╝→╚S.═║═║══╣
                ║                     ║ ║    ║ ╠Category╝→╚S.╣
                ╚═══════Flag══════════╝ ╚Flag╝ ╚════Flag═════╝

@docs Location, parseLocation
@docs Flag, Category, SearchTerm

-}

import Browser
import Maybe.Extra as Maybe
import Set exposing (Set)
import Set.Extra as Set
import String.Extra as String
import Url exposing (Url)



---- Update ----


{-| Handles your message type (`modelMsg`) as well as changes to the Ui state (Url).
-}
type Msg modelMsg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | AppMsg modelMsg
    | UrlCmd Link


{-| Relative changes to the Url are only applied when a link opens in the same tab.
Otherwise, absolute changes are applied by the Elm runtime.

**Example:**

The user clicks a menu (implemented as a `Toggle`) to open it, then clicks again to close it.

(a)
They decide to share the menu, so they right-click on the toggle and choose 'copy link'.
Their friend opens the link and the handle is activated. Otherwise, the friend's Ui is at the default
state. A toggle's `href` always points to the `open` state.

(b)
They copy the Url and paste it in another tab or browser or device.
The app loads and restores exactly the same Ui state as in the original tab.

  - Jump Navigation
      - [x]  [With a single target (goTo)](Less-Ui-Html#goTo)
      - [x]  [With two back-and-forth targets (bounce)](Less-Ui-Html#bounce)

  - Progressive Disclosure
      - [x]  [Orthogonal; any number can be active (toggle)](Less-Ui-Html#toggle)
      - [ ] Exactly one active at a given Ui node (tab) ☞ [#8](https://github.com/upsiflu/restrictive/issues/8) ☞ [#2](https://github.com/upsiflu/restrictive/issues/2)
      - [ ] One or zero active in the browser tab (dropdown, dialog, popover) ☞ [#8](https://github.com/upsiflu/restrictive/issues/8)

  - Dynamic Links
      - [x]  [Search](Less-Ui-Html#search) or [Filter](Less-Ui-Html#filter) a [Category](#Category)

-}
type Link
    = GoTo ParsedLocation
    | Toggle Flag
    | Bounce
        { there : ParsedLocation
        , here : ParsedLocation
        }
    | Filter
        { category : Category
        , searchTerm : SearchTerm
        }


{-| This happens when a Link is clicked. We want to apply the relative change here.
-}
fromUrl : Url -> Link
fromUrl url =
    let
        bounce : () -> Maybe Link
        bounce () =
            case
                getStateSearchTerms "bounce" url
                    |> List.head
                    |> Maybe.map (String.split "<>")
            of
                Just (there :: here) ->
                    Just
                        (Bounce
                            { there = parseLocation (String.replace "%23" "#" there)
                            , here =
                                String.join "<>" here
                                    |> String.replace "%23" "#"
                                    |> parseLocation
                            }
                        )

                _ ->
                    Nothing

        goTo : () -> Link
        goTo () =
            String.dropLeft 1 url.path
                ++ (Maybe.map (\fragment -> "#" ++ fragment) url.fragment
                        |> Maybe.withDefault ""
                   )
                |> parseLocation
                |> GoTo

        toggle : () -> Maybe Link
        toggle () =
            getStateSearchTerms "toggle" url
                |> List.head
                |> Maybe.map Toggle
    in
    toggle ()
        |> Maybe.orElseLazy bounce
        |> Maybe.withDefault (goTo ())


{-| Use with `Html.Attributes.href`. Will not include the current state. `Toggle` and `Bounce` will produce an extra flag enabling switchy changes.
-}
toHref : Link -> String
toHref relativeChange =
    case relativeChange of
        GoTo location ->
            encodeLocation location

        Toggle flag ->
            "?" ++ flag ++ "&toggle=" ++ flag

        Bounce parsedLocations ->
            let
                ( there, here ) =
                    ( encodeLocation parsedLocations.there, encodeLocation parsedLocations.here )
            in
            there ++ "?bounce=" ++ String.replace "#" "%23" (there ++ "<>" ++ here)

        Filter filter ->
            "?" ++ filter.category ++ "=" ++ filter.searchTerm


{-| Marks the delta from the previous to the current state. A String can describe the Aria-"set" (such as "page") that the link refers to.
-}
type Mutation
    = StateEntered String
    | StateInside String
    | StateLeft String
    | StateOutside String
    | SwitchedOn
    | StillOn
    | SwitchedOff
    | StillOff


{-| Probe the delta between two states against a [Link](#Link) and, with an optional Aria set name such as "page", generate a [Mutation](#Mutation).
-}
mutationFromTwoStates : { current : State, previous : Maybe State } -> Link -> Maybe String -> Mutation
mutationFromTwoStates { current, previous } link maybeSetName =
    let
        predicate : State -> Bool
        predicate state =
            case link of
                GoTo location ->
                    locationIsSubsetOf state location

                Toggle flag ->
                    Maybe.withDefault "" (Maybe.andThen Url.percentDecode state.query)
                        |> String.split "&"
                        |> List.member flag

                Bounce { here } ->
                    locationIsSubsetOf state here

                Filter { category } ->
                    Maybe.withDefault "" state.query
                        |> (\q -> String.startsWith (category ++ "=") q || String.contains ("&" ++ category ++ "=") q)

        locationIsSubsetOf : State -> ParsedLocation -> Bool
        locationIsSubsetOf state location =
            case ( location, getLocation state ) of
                ( OnlyPath innerPath, OnlyPath path ) ->
                    String.contains innerPath path

                ( OnlyPath innerPath, PathAndFragment path _ ) ->
                    String.contains innerPath path

                ( OnlyFragment innerFragment, PathAndFragment _ fragment ) ->
                    innerFragment == fragment

                ( PathAndFragment innerPath innerFragment, PathAndFragment path fragment ) ->
                    innerFragment == fragment && String.contains innerPath path

                _ ->
                    False

        getLocation : State -> ParsedLocation
        getLocation url =
            case ( getPath url, getFragment url ) of
                ( path, Just fragment ) ->
                    PathAndFragment path fragment

                ( path, Nothing ) ->
                    OnlyPath path

        getFragment : State -> Maybe Fragment
        getFragment =
            .fragment

        getPath : State -> Path
        getPath { path } =
            String.dropLeft 1 path
    in
    case ( predicate current, predicate (Maybe.withDefault current previous), maybeSetName ) of
        ( True, True, Just set ) ->
            StateInside set

        ( True, True, Nothing ) ->
            StillOn

        ( True, False, Just set ) ->
            StateEntered set

        ( True, False, Nothing ) ->
            SwitchedOn

        ( False, True, Just set ) ->
            StateLeft set

        ( False, True, Nothing ) ->
            SwitchedOff

        ( False, False, Just set ) ->
            StateOutside set

        ( False, False, Nothing ) ->
            StillOff


{-| Think `update` in TEA.
-}
apply : Link -> State -> ( { pushHistoryState : Bool }, State )
apply link =
    let
        mapFlags : (Set Flag -> Set Flag) -> State -> State
        mapFlags fu state =
            { state
                | query =
                    Maybe.toList state.query
                        |> List.concatMap (String.split "&")
                        |> Set.fromList
                        |> fu
                        |> Set.toList
                        |> String.join "&"
                        |> String.nonEmpty
            }

        replaceAssignment : Category -> SearchTerm -> State -> State
        replaceAssignment category searchTerm =
            mapFlags (Set.filter (String.startsWith (category ++ "=") >> not))
                >> (Set.insert >> mapFlags) (category ++ "=" ++ searchTerm)

        setFragment : Fragment -> State -> State
        setFragment fragment state =
            { state | fragment = Just fragment }

        setPath : Path -> State -> State
        setPath path state =
            { state | path = "/" ++ path }

        withHistory : b -> ( { pushHistoryState : Bool }, b )
        withHistory =
            Tuple.pair { pushHistoryState = True }

        withoutHistory : b -> ( { pushHistoryState : Bool }, b )
        withoutHistory =
            Tuple.pair { pushHistoryState = True }
    in
    case link of
        GoTo parsedLocation ->
            withHistory
                << (case parsedLocation of
                        OnlyPath path ->
                            setPath path

                        OnlyFragment fragment ->
                            setFragment fragment

                        PathAndFragment path fragment ->
                            setPath path >> setFragment fragment
                   )

        Toggle flag ->
            mapFlags (Set.remove ("toggle=" ++ flag) >> Set.toggle flag)
                >> withoutHistory

        Bounce parsedLocations ->
            let
                ( there, here ) =
                    ( encodeLocation parsedLocations.there, encodeLocation parsedLocations.here )
            in
            withHistory
                << (\state ->
                        if String.startsWith there (String.dropLeft 1 state.path) then
                            setPath here state

                        else
                            setPath there state
                   )

        Filter { category, searchTerm } ->
            replaceAssignment category searchTerm
                >> withoutHistory


{-| **Progressive disclosure**: Turning off a `Flag` renders all corresponding bits
of `Ui` invisible.
Example: [Ui.Html#toggle](Less-Ui-Html#toggle).

**Parametric search:** Assignments such as `?a=b` may represent currently active Tabs
or a search string.
Example: [Ui.Html#search](Less-Ui-Html#search).

-}
type alias Flag =
    String


{-| **Pages:** Jump through a `/`-delimited tree or a pool of exclusive pages. Example: [Ui.Html#bounce](Less-Ui-Html#goTo)

**Global Tabs**: Navigate between tabs using the global `nav` bar.

**Items in a Set:** A path may represent a single item or group. Navigating to an Item will scroll it into the viewport.

**Items in a Tree**: Here, each item has a `parent` item, which is activated when its link is clicked for the second time. Example: [Ui.Html#bounce](Less-Ui-Html#bounce)

-}
type alias Path =
    String


{-| (`/`)`<Path>`(``` #``<Fragment> ```)
-}
type alias Location =
    String


type ParsedLocation
    = OnlyPath Path
    | OnlyFragment Fragment
    | PathAndFragment Path Fragment


{-|

    parseLocation ""
        -> OnlyPath ""

    parseLocation "/"
        -> OnlyPath ""

    parseLocation "#"
        -> OnlyFragment ""

    parseLocation "/#"
        -> PathAndFragment "" ""

-}
parseLocation : Location -> ParsedLocation
parseLocation location =
    case String.split "#" location of
        [] ->
            OnlyPath ""

        "" :: fragment ->
            OnlyFragment (String.join "#" fragment)

        [ path ] ->
            OnlyPath path

        path :: fragment ->
            PathAndFragment path (String.join "#" fragment)


encodeLocation : ParsedLocation -> Location
encodeLocation parsedLocation =
    case parsedLocation of
        OnlyPath path ->
            "/" ++ path

        OnlyFragment fragment ->
            "#" ++ fragment

        PathAndFragment path fragment ->
            -- Todo: Tail Call Optimize
            encodeLocation (OnlyPath path) ++ encodeLocation (OnlyFragment fragment)


{-| Note that a `Category` may contain "=":

    import Url

    Url.fromString "http://test/?a=b=c"
        |> Maybe.map (getStateSearchTerms "a")
        --> Just ["b=c"]

    Url.fromString "http://test/?a=b=c"
        |> Maybe.map (getStateSearchTerms "a=")
        --> Just []

    Url.fromString "http://test/?a=b=c"
        |> Maybe.map (getStateSearchTerms "a=b")
        --> Just ["c"]

Attention: Categories "toggle" and "bounce" are interpreted as redirect directives in relative links ([-> Link](#Link)).

-}
getStateSearchTerms : Category -> Url -> List SearchTerm
getStateSearchTerms category =
    .query
        >> Maybe.map
            (String.split "&"
                >> List.filter (String.startsWith (category ++ "="))
            )
        >> Maybe.withDefault []
        >> List.map (String.dropLeft (String.length category + 1))


{-| Distinguish parallel search inputs on a screen.
-}
type alias Category =
    String


{-| Represent a filter value or the search input.
-}
type alias SearchTerm =
    String


{-| "Graphical Web browsers typically scroll to position pages so that the top of the element
identified by the fragment id is aligned with the top of the viewport;
thus fragment identifiers are often used in tables of content and in permalinks.
The appearance of the identified element can be changed through
the `:target` CSS pseudoclass; Wikipedia uses this to highlight the selected reference."
(from Wikipedia)

The **target** is somewhat tied to the **focus**, but when updating the fragment (hash) via Elm,
the Browser may not update the focus, so it's safer to add a `focus-me` custom element to the DOM.

-}
type alias Fragment =
    String



---- Create State ----


{-|

  - path : String
  - query : Maybe String
  - fragment : Maybe String

-}
type alias State =
    Url
