module Restrictive.Link exposing
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
import List.Extra as List
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

  - Jump Navigation
      - [x]  [With a single target (goTo)](#goTo)
      - [x]  [With two back-and-forth targets (bounce)](#bounce)

  - Progressive Disclosure
      - [x]  [Orthogonal; any number can be active (toggle)](#toggle)
      - [ ] Exactly one active at a given Ui node (tab) ☞ [#8](https://github.com/upsiflu/restrictive/issues/8) ☞ [#2](https://github.com/upsiflu/restrictive/issues/2)
      - [ ] One or zero active in the browser tab (dropdown, dialog) ☞ [#8](https://github.com/upsiflu/restrictive/issues/8)

  - Dynamic Links
      - [x]  [Search or Filter](#filter) a [Category](#Category) by [user-input Data](#Data)

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


{-| -}
fromUrl : Url -> Maybe Link
fromUrl url =
    let
        getLastSearchTermOf : Category -> State -> Maybe SearchTerm
        getLastSearchTermOf category =
            .query
                >> Maybe.andThen
                    (String.split "&"
                        >> List.find (String.startsWith (category ++ "="))
                    )
    in
    case getLastSearchTermOf "toggle" url of
        Just flag ->
            Just (Toggle flag)

        Nothing ->
            case getLastSearchTermOf "bounce" url of
                Just escapedLocation ->
                    case String.split "<>" escapedLocation of
                        there :: here ->
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

                _ ->
                    Nothing


{-| -}
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


{-| -}
type Mutation
    = StateEntered String
    | StateInside String
    | StateLeft String
    | StateOutside String
    | SwitchedOn
    | StillOn
    | SwitchedOff
    | StillOff


{-| -}
mutationFromTwoStates : { current : State, previous : Maybe State } -> Link -> Maybe String -> Mutation
mutationFromTwoStates { current, previous } link maybeSetName =
    let
        {-

           import Set
           import Url

           Url.fromString "http://a/?x"
               |> Maybe.map (stateHasFlag "y")
               --> Just False

           Url.fromString "http://a/?y&z"
               |> Maybe.map (stateHasFlag "y")
               --> Just True

           Url.fromString "http://a/?y="
               |> Maybe.map (stateHasFlag "y")
               --> Just False

           Url.fromString "http://a/?y="
               |> Maybe.map (stateHasFlag "y=")
               --> Just True

           Url.fromString "http://a/?y=1&y=2&y=3"
               |> Maybe.map (stateHasFlag "y=")
               --> Just False

           Url.fromString "http://a/?y=1&y=2&y=3"
               |> Maybe.map (stateHasFlag "y=2")
               --> Just True

        -}
        predicate : State -> Bool
        predicate state =
            case link of
                GoTo location ->
                    locationIsSubsetOf state location

                Toggle flag ->
                    Maybe.withDefault "" state.query
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
                    innerPath == path

                ( OnlyPath innerPath, PathAndFragment path _ ) ->
                    innerPath == path

                ( OnlyFragment innerFragment, PathAndFragment _ fragment ) ->
                    innerFragment == fragment

                ( PathAndFragment innerPath innerFragment, PathAndFragment path fragment ) ->
                    innerFragment == fragment && innerPath == path

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


{-| -}
apply : Link -> State -> ( { pushHistoryState : Bool }, State )
apply link =
    let
        mapFlags : (Set Flag -> Set Flag) -> State -> State
        mapFlags fu state =
            { state
                | query =
                    Maybe.withDefault "" state.query
                        |> String.split "&"
                        |> Set.fromList
                        |> fu
                        |> Set.toList
                        |> String.join "&"
                        |> String.nonEmpty
            }

        replaceAssignment : Category -> SearchTerm -> State -> State
        replaceAssignment category searchTerm =
            mapFlags (Set.filter (String.startsWith (category ++ "=") >> not))
                >> insertFlag (category ++ "=" ++ searchTerm)

        insertFlag : Flag -> State -> State
        insertFlag =
            Set.insert >> mapFlags

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
            mapFlags (Set.toggle flag)
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
Example: [Layout.Html#toggle](Restrictive.Layout.Html#toggle).

**Parametric search:** Assignments such as `?a=b` may represent currently active Tabs
or a search string.
Example: [Layout.Html#search](Restrictive.Layout.Html#search).

-}
type alias Flag =
    String


{-| **Pages:** Jump through a `/`-delimited tree or a pool of exclusive pages. Example: [Layout.Html#bounce](Restrictive.Layout.Html#goTo)

**Global Tabs**: Navigate between tabs using the global `nav` bar.

**Items in a Set:** A path may represent a single item or group. Navigating to an Item will scroll it into the viewport.

**Items in a Tree**: Here, each item has a `parent` item, which is activated when its link is clicked for the second time. Example: [Layout.Html#bounce](Restrictive.Layout.Html#bounce)

-}
type alias Path =
    String


{-| (`/`)[`<Path>`](#Path)`#`[`<Fragment>`](#Fragment)
-}
type alias Location =
    String


{-| -}
type ParsedLocation
    = OnlyPath Path
    | OnlyFragment Fragment
    | PathAndFragment Path Fragment


{-| -}
parseLocation : Location -> ParsedLocation
parseLocation location =
    case String.split "#" location of
        "" :: fragment ->
            OnlyFragment (String.join "#" fragment)

        path :: fragment ->
            PathAndFragment path (String.join "#" fragment)

        _ ->
            OnlyPath location


encodeLocation : ParsedLocation -> Location
encodeLocation parsedLocation =
    case parsedLocation of
        OnlyPath path ->
            "/" ++ path

        OnlyFragment fragment ->
            "#" ++ fragment

        PathAndFragment path fragment ->
            encodeLocation (OnlyPath path) ++ encodeLocation (OnlyFragment fragment)


{-| returns True if the inner location string is a subset of the given outer Url's location
-}
getStateAssignmentFlags : Category -> Url -> List Flag
getStateAssignmentFlags category =
    .query
        >> Maybe.map
            (String.split "&"
                >> List.filter (String.startsWith (category ++ "="))
            )
        >> Maybe.withDefault []


{-| returns True if the inner location string is a subset of the given outer Url's location
-}
getStateSearchTerms : Category -> Url -> List SearchTerm
getStateSearchTerms category =
    getStateAssignmentFlags category
        >> List.map (stripPrefix "=")


{-| Distinguish parallel search inputs on a screen.
-}
type alias Category =
    String


{-| Can represent a filter value or the search input.
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


{-| -}
type alias State =
    Url



---- Deconstruct State ----
---- Apply ----


stripPrefix : String -> String -> String
stripPrefix prefix str =
    case String.split prefix str of
        _ :: r :: est ->
            String.join prefix (r :: est)

        _ ->
            str
