module Restrictive.Link exposing
    ( Link
    , goTo, bounce, toggle, filter, err
    , fromTransition
    , makeRelative
    , Msg(..), UrlCmd(..), stateIntegrateUrlCmd
    , isActive
    , getCurrentData, Data(..)
    , toString
    , Flag, Path, Assignment, Category, SearchTerm, Fragment
    , State, stateToString, Transition
    , Query
    , view
    , Style, mapStyle
    , Templates
    , applyTransition
    , upTo, querySerialiseLocation, queryParseLocation, stateFromString
    , stateSetPath, stateSetFragment, stateTurnOnFlag, stateToggleFlag
    , stateAddAssignment, stateRemoveAssignments
    , stateHasFlag, getStateFragment, getStatePath, getStateLocation
    )

{-| We use the Url query to keep track of the Ui state. This makes sharing a Ui state as easy as copying the Url.

@docs Link


# Create

  - Jump Navigation
      - [x]  [With a single target (goTo)](#goTo)
      - [x]  [With two back-and-forth targets (bounce)](#bounce)

  - Progressive Disclosure
      - [x]  [Orthogonal; any number can be active (toggle)](#toggle)
      - [ ] Exactly one active at a given Ui node (tab) ☞ [#8](https://github.com/upsiflu/restrictive/issues/8) ☞ [#2](https://github.com/upsiflu/restrictive/issues/2)
      - [ ] One or zero active in the browser tab (dropdown, dialog) ☞ [#8](https://github.com/upsiflu/restrictive/issues/8)

  - Dynamic Links
      - [x]  [Search or Filter](#filter) a [Category](#Category) by [user-input Data](#Data)

---

@docs goTo, bounce, toggle, filter, err

@docs fromTransition


# Map

@docs makeRelative


# Msg

@docs Msg, UrlCmd, stateIntegrateUrlCmd


# Query

@docs isActive


# Deconstruct

@docs getCurrentData, Data
@docs toString

@docs Flag, Path, Assignment, Category, SearchTerm, Fragment


# State transitions

@docs State, stateToString, Transition


# Query and Deconstruct State

@docs Query


# View

@docs view
@docs Style, mapStyle
@docs Templates


# Apply

@docs applyTransition


# Internals

Test these functions with `elm-verify-examples` (see the Readme)

@docs upTo, querySerialiseLocation, queryParseLocation, stateFromString

@docs stateSetPath, stateSetFragment, stateTurnOnFlag, stateToggleFlag
@docs stateAddAssignment, stateRemoveAssignments

@docs stateHasFlag, getStateFragment, getStatePath, getStateLocation

-}

import Browser
import List.Extra as List
import Maybe.Extra as Maybe
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout.Region exposing (OrHeader(..))
import Result.Extra as Result
import Set exposing (Set)
import Set.Extra as Set
import String.Extra as String
import Url exposing (Url)
import Url.Codec exposing (Codec, ParseError(..))


{-| Encodes an intended transition of [the Ui State](#State)
-}
type Link
    = GoTo ( Maybe Path, Fragment )
    | Bounce { isAbsolute : Bool } { there : ( Maybe Path, Fragment ), here : ( Maybe Path, Fragment ) }
    | Toggle { isAbsolute : Bool } Flag
    | Filter Assignment
    | ErrorMessage String



---- Create ----


{-| Change the path in the Url for the next `view`

    a   -> b -> b
    a?f -> b -> b?f

```css
a:active, a:link:active, a:visited:active {}
```

Note that `(Nothing, Nothing)` is coerced into `(Just "", Nothing)` (is it What happens if not?)

-}
goTo : ( Maybe Path, Fragment ) -> Link
goTo =
    GoTo


{-| If you are `there`, go `here`, else go `there`.
Use for expanding/collapsing nodes, or for tree-shaped scenes in general (in which case, "here" is the parent path).

`Bounce {"there" "here"}` yields an `Internal` request of `there?reroute=here`

    a -> b?reroute=c -> b
    c -> b?reroute=c -> b
    b -> b?reroute=c -> c

```css
a:active, a:link:active, a:visited:active {}
a.bounce {}
```

-}
bounce : { there : ( Maybe Path, Fragment ), here : ( Maybe Path, Fragment ) } -> Link
bounce =
    Bounce { isAbsolute = True }


{-| Turn a flag on or off

    a?f -> ?toggle=f -> a

```css
a[role="switch"]:aria-checked {}
```

-}
toggle : Flag -> Link
toggle =
    Toggle { isAbsolute = True }


{-| -}
filter : Assignment -> Link
filter =
    Filter


{-| -}
err : String -> Link
err =
    ErrorMessage


{-| Generates a Link, given the current path.

Note that in Elm, an absolute Url path always starts with a slash.
To check if the path is intended to be relative, we compare it with the previous path first.
This is not absolutely necessary because the Elm runtime does the same check. However, it gives terser and more expressive Hrefs.

In the following tests, we assume a previous path of "/"

    import Url


    testFromUrl : String -> Link
    testFromUrl str =
        Maybe.map2 fromTransition
            (Url.fromString ("http://localhost"++str))
            (Url.fromString ("http://localhost/"))
                |> Maybe.withDefault (err "Url.fromString failed")

    --Bounce

    (testFromUrl) "/there?reroute=here~f2#f1"
        -->  bounce { there = (Just "there", Just "f1"), here = (Just "here", Just "f2") } |> makeRelative


    (testFromUrl) "/there?reroute=/~f2#f1"
        -->  bounce { there = (Just "there", Just "f1"), here = (Just "", Just "f2") } |> makeRelative

    (testFromUrl) "/there?reroute=~f2#f1"
        -->  bounce { there = (Just "there", Just "f1"), here = (Nothing, Just "f2") } |> makeRelative


    (testFromUrl) "/there?reroute=#f1"
        -->  bounce  { there = (Just "there", Just "f1"), here = (Nothing, Nothing) } |> makeRelative


    (testFromUrl) "/?reroute=here~f2#f1"
        --> bounce { there = (Nothing, Just "f1"), here = (Just "here", Just "f2") } |> makeRelative


    (testFromUrl) "/?reroute=here~f2"
        --> bounce  { there = (Nothing, Nothing), here = (Just "here", Just "f2") } |> makeRelative


    (testFromUrl) "/?reroute=~"
        --> bounce { there = (Nothing, Nothing), here = (Nothing, Nothing) } |> makeRelative



    --Toggle

    (testFromUrl) "?toggle=flag"
        --> toggle "flag" |> makeRelative

    (testFromUrl) "?toggle=flag&!"
        --> toggle "flag"


    --GoTo

    testFromUrl "/path#"
        --> goTo (Just "path", Just "")

    testFromUrl "/path#fragment"
        --> goTo (Just "path", Just "fragment")

    testFromUrl "/path"
        --> goTo (Just "path", Nothing)

    testFromUrl "/path/"
        --> goTo (Just "path", Nothing)

    testFromUrl "/#fragment"
        --> goTo (Nothing, Just "fragment")

    testFromUrl "/#?fragment"
        --> goTo (Nothing, Just "?fragment")

-}
fromTransition : State -> State -> Link
fromTransition new current =
    Url.Codec.parseUrl codecs
        { new
            | path =
                if current.path == new.path then
                    ""

                else
                    new.path
        }
        |> Result.extract fromParseError


fromParseError : ParseError -> Link
fromParseError e =
    ErrorMessage <|
        String.join ", " <|
            case e of
                SegmentMismatch { expected, available } ->
                    [ "SegmentMismatch: expected", expected, "available", available ]

                SegmentNotAvailable ->
                    [ "SegmentNotAvailable" ]

                DidNotConsumeEverything strList ->
                    "DidNotConsumeEverything" :: strList

                NeededSingleQueryParameterValueGotMultiple { key, got } ->
                    [ "NeededSingleQueryParameterValueGotMultiple", key ] ++ got

                _ ->
                    [ "other link parse error" ]



---- Map ----


{-| By default, links render absolute, which means they don't respect
the current state. This is the desired behavior when sharing links.

For example, if you share a toggle, you want the receiver to
experience the "on" state; for a bounce, you want the "there" location,
regardless of the current state of their application.

When clicking a link in an application, a transition relative to the
current state is desired, so `Application.update` makes internal links relative.

-}
makeRelative : Link -> Link
makeRelative link =
    case link of
        Bounce _ location ->
            Bounce { isAbsolute = False } location

        Toggle _ flag ->
            Toggle { isAbsolute = False } flag

        _ ->
            link



---- Update ----


{-| Handles your message type (`modelMsg`) as well as changes to the Ui state (Url).
-}
type Msg modelMsg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | AppMsg modelMsg
    | UrlCmds (List UrlCmd)



---- Query ----


{-| -}
isActive : State -> Link -> Bool
isActive state l =
    case l of
        GoTo ( maybePath, _ ) ->
            maybePath == Just state.path

        Bounce _ { there } ->
            isActive state (GoTo there)

        Toggle _ f ->
            stateHasFlag f state

        Filter ( category, _ ) ->
            stateHasCategory category state

        ErrorMessage m ->
            stateHasError m state



---- Deconstruct ----


{-| -}
getCurrentData : State -> Link -> Maybe Data
getCurrentData url l =
    case l of
        GoTo _ ->
            Nothing

        Bounce _ _ ->
            Nothing

        Toggle _ _ ->
            Nothing

        Filter ( category, _ ) ->
            Maybe.map Filtered (getStateSearchTermOf category url)

        ErrorMessage _ ->
            Nothing


getDestination : Link -> Maybe ( Maybe Path, Fragment )
getDestination link =
    case link of
        GoTo location ->
            Just location

        Bounce _ { there } ->
            Just there

        _ ->
            Nothing


getPath : Link -> Maybe Path
getPath =
    getDestination
        >> Maybe.andThen Tuple.first
        >> serialisePath
        >> Just


getFragment : Link -> Fragment
getFragment =
    getDestination
        >> Maybe.andThen Tuple.second


{-| Try to create an UrlString.

    --Bounce

    bounce
        { there = (Just "there", Just "f1")
        , here = (Just "here", Just "f2")
        }
        |> makeRelative
        |> toString
        --> "/there?reroute=here~f2#f1"

    bounce
        { there = (Just "there", Just "f1")
        , here = (Just "", Just "f2")
        }
        |> makeRelative
        |> toString
        --> "/there?reroute=/~f2#f1"

    bounce
        { there = (Just "there", Just "f1")
        , here = (Just "", Nothing)
        }
        |> makeRelative
        |> toString
        --> "/there?reroute=/#f1"

    bounce
        { there = (Just "", Just "f1")
        , here = (Just "here", Just "f2")
        }
        |> makeRelative
        |> toString
        --> "/?reroute=here~f2#f1"

    bounce
        { there = (Just "", Nothing)
        , here = (Just "here", Just "f2")
        }
        |> makeRelative
        |> toString
        --> "/?reroute=here~f2"

    bounce
        { there = (Nothing, Just "f1")
        , here = (Just "here", Just "f2")
        }
        |> makeRelative
        |> toString
        --> "?reroute=here~f2#f1"

    bounce
        { there = (Nothing, Nothing)
        , here = (Just "here", Just "f2")
        }
        |> makeRelative
        |> toString
        --> "?reroute=here~f2"


    --Toggle

    toggle "flag"
        |> makeRelative
        |> toString
        --> "?toggle=flag"

    toggle "flag"
        |> toString
        --> "?toggle=flag&!"


    --GoTo

    goTo (Just "path", Just "fragment")
        |> toString
        --> "/path#fragment"

    goTo (Just "path", Nothing)
        |> toString
        --> "/path"

    goTo (Nothing, Just "fragment")
        |> toString
        --> "#fragment"

    goTo (Nothing, Nothing)
        |> toString
        --> ""

    goTo (Nothing, Just "")
        |> toString
        --> "#"

-}
toString : Link -> String
toString =
    Url.Codec.toString codecs
        >> Maybe.andThen Url.percentDecode
        >> Maybe.withDefault "?errorMessage=Error converting link to string"


{-| The data that a link emits in the context of a given Url.
-}
type Data
    = Filtered String


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


{-| Distinguish parallel search inputs on a screen.
-}
type alias Category =
    String


{-| Can represent a filter value or the search input.
-}
type alias SearchTerm =
    String


{-| **Viewmode**: Assign a value to a key.

**Search**: Assign a searchTerm to a category.

Assignments are parallelly treated as Flags! So `?a=b` is both an assignment `(a, b)`
and a flag `"a=b"`.

-}
type alias Assignment =
    ( Category, SearchTerm )


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
    Maybe String


{-| Manage the assignments your app keeps in the Url.

`Set (a, b)` will delete all instances of `a=..` and replace them with `a=b`
`Add (a, b)` will append `a=b` to the end if it's not yet there
`Clear (a, _)` will delete all instances of `a=..`

**Outlook:** In the longer run, this type supports any message we want to send from
the application to the restrictive framework.

-}
type UrlCmd
    = Set Assignment
    | Add Assignment
    | Clear Category



---- Create State ----


{-| -}
type alias State =
    Url


{-| Only use for testing!
-}
stateFromString : String -> Maybe State
stateFromString =
    Url.fromString



---- Create State Transitions ----


{-| -}
type alias Transition =
    State -> State


{-| -}
stateSetPath : Path -> Transition
stateSetPath path state =
    { state | path = "/" ++ path }


{-| -}
stateSetFragment : Fragment -> Transition
stateSetFragment fragment state =
    { state | fragment = fragment }


{-| -}
stateSetLocation : ( Maybe Path, Fragment ) -> Transition
stateSetLocation destination =
    case destination of
        ( Just path, fragment ) ->
            stateSetPath path >> stateSetFragment fragment

        ( Nothing, fragment ) ->
            stateSetFragment fragment


{-| -}
stateTurnOnFlag : Flag -> Transition
stateTurnOnFlag flag =
    if flag == "" then
        identity

    else
        stateMapQuery <| \q -> { q | flags = Set.insert flag q.flags }


{-|

    import Url exposing (Url)

    testQuery : String -> (Url -> Url) -> String
    testQuery query fu =
        "http://localhost/?" ++ query
            |> Url.fromString
            |> Maybe.andThen (fu >> .query)
            |> Maybe.withDefault "Url.fromString or .query failed"


    stateToggleFlag "g"
       |> testQuery "f&g&h&a=b&c=d=e"
                --> "f&h&a=b&c=d=e"


    stateToggleFlag "g"
       |> testQuery "f&h&a=b&c=d=e"
                --> "f&g&h&a=b&c=d=e"

-}
stateToggleFlag : Flag -> Transition
stateToggleFlag flag =
    if flag == "" then
        identity

    else
        stateMapQuery <| \q -> { q | flags = Set.toggle flag q.flags }


{-| -}
stateIntegrateUrlCmd : UrlCmd -> Transition
stateIntegrateUrlCmd urlCommand =
    case urlCommand of
        Set assignment ->
            stateReplaceAssignment assignment

        Add assignment ->
            stateAddAssignment assignment

        Clear k ->
            stateRemoveAssignments [ k ]


{-| -}
stateAddAssignment : Assignment -> Transition
stateAddAssignment assignment =
    stateMapQuery <| \q -> { q | assignments = assignment :: q.assignments }


{-| -}
stateReplaceAssignment : Assignment -> Transition
stateReplaceAssignment (( category, _ ) as assignment) =
    stateMapQuery <| \q -> { q | assignments = assignment :: List.filter (\( key, _ ) -> key /= category) q.assignments }


{-| -}
stateRemoveAssignments : List Category -> Transition
stateRemoveAssignments categories =
    stateMapQuery <|
        \q ->
            { q
                | assignments =
                    List.filter
                        (Tuple.first >> (\key -> not (List.member key categories)))
                        q.assignments
            }


stateMapQuery : (Query -> Query) -> Transition
stateMapQuery fu state =
    { state | query = (parseQueryString >> fu >> serializeQuery) state.query }



---- Query State ----


{-|

    import Set
    import Url

    "http://a/?y"
        |> Url.fromString
        |> Maybe.map (stateHasFlag "z")
        --> Just False

    "http://a/?y&z"
        |> Url.fromString
        |> Maybe.map (stateHasFlag "z")
        --> Just True

-}
stateHasFlag : Flag -> State -> Bool
stateHasFlag flag =
    getStateFlags >> List.member flag


stateHasAssignment : Assignment -> State -> Bool
stateHasAssignment ( key, value ) =
    stateHasFlag (key ++ "=" ++ value)


stateHasError : String -> State -> Bool
stateHasError m =
    stateHasAssignment ( "e", m )


stateHasCategory : Category -> State -> Bool
stateHasCategory category =
    getStateFlags >> List.filterMap (String.split "=" >> List.head) >> List.member category



---- Deconstruct State ----


{-| -}
stateToString : State -> String
stateToString =
    Url.toString


{-| -}
getStateFragment : State -> Fragment
getStateFragment =
    .fragment


{-| -}
getStatePath : State -> Path
getStatePath { path } =
    String.dropLeft 1 path


{-| -}
getStateLocation : State -> ( Maybe Path, Fragment )
getStateLocation url =
    ( String.nonEmpty (getStatePath url), getStateFragment url )


{-| -}
type alias Query =
    { flags : Set Flag, assignments : List Assignment }


getStateFlags : State -> List Flag
getStateFlags =
    .query >> parseQueryString >> .flags >> Set.toList


getStateAssignments : State -> List Assignment
getStateAssignments =
    .query >> parseQueryString >> .assignments


getStateLastAssignmentOf : Category -> State -> Maybe Assignment
getStateLastAssignmentOf category =
    getStateAssignments
        >> List.find (\( key, _ ) -> key == category)


getStateSearchTermOf : Category -> State -> Maybe SearchTerm
getStateSearchTermOf category =
    getStateLastAssignmentOf category
        >> Maybe.map Tuple.second



---- View ----
-- {-| -}
-- preset :
--     { global : List (Html.Attribute Never) -> List (Html Never) -> ( CustomHtml (Html msg) (Html.Attribute msg), Renderer aspect (Html msg) (Html.Attribute msg) )
--     , inline : List (Html.Attribute Never) -> List (Html Never) -> ( CustomHtml (Html msg) (Html.Attribute msg), Renderer aspect (Html msg) (Html.Attribute msg) )
--     , nav : List (Html.Attribute Never) -> List (Html Never) -> ( CustomHtml (Html msg) (Html.Attribute msg), Renderer aspect (Html msg) (Html.Attribute msg) )
--     , tab : List (Html.Attribute Never) -> List (Html Never) -> ( CustomHtml (Html msg) (Html.Attribute msg), Renderer aspect (Html msg) (Html.Attribute msg) )
--     }
-- preset =
--     { global = \att con -> ( defaultHtml, { empty | attributes = List.map (Attr.map never) att, label = List.map (Html.map never) con, isInline = False } )
--     , inline = \att con -> ( defaultHtml, { empty | attributes = List.map (Attr.map never) att, label = List.map (Html.map never) con, isInline = True } )
--     , nav = \att con -> ( { defaultHtml | element = Html.input }, { empty | attributes = Attr.type_ "radio" :: List.map (Attr.map never) att, label = List.map (Html.map never) con, isInline = False } )
--     , tab = \att con -> ( { defaultHtml | element = Html.input }, { empty | attributes = Attr.type_ "radio" :: List.map (Attr.map never) att, label = List.map (Html.map never) con, isInline = True } )
--     }
-- Reformulate link and switch as Wrapper? No because then one could `wrap` in a link or a switch.
-- {-| textual header link
-- -}
-- headerLink : Link -> (( OrHeader region, Url ) -> { linkHtml : Get (OrHeader region) (List ( String, Html msg )), occlude : Mask region a })
-- headerLink link =
--     let
--         ( customHtml, renderer ) =
--             preset.global [] [ Html.text (toId link) ]
--     in
--     view customHtml renderer link
-- {-| textual inline link
-- -}
-- inlineLink : Link -> (( OrHeader region, Url ) -> { linkHtml : Get (OrHeader region) (List ( String, Html msg )), occlude : Mask region a })
-- inlineLink link =
--     let
--         ( customHtml, renderer ) =
--             preset.inline [] [ Html.text (toId link) ]
--     in
--     view customHtml renderer link
-- {-| inline link with custom Html
-- -}
-- inlineLink_ : CustomHtml html attribute -> List attribute -> List html -> Link -> (( OrHeader region, Url ) -> { linkHtml : Get (OrHeader region) (List ( String, html )), occlude : Mask region a })
-- inlineLink_ customHtml att con =
--     view customHtml { empty | attributes = att, label = con, isInline = True }
-- {-| header link with custom Html
-- -}
-- headerLink_ : CustomHtml html attribute -> List attribute -> List html -> Link -> (( OrHeader region, Url ) -> { linkHtml : Get (OrHeader region) (List ( String, html )), occlude : Mask region a })
-- headerLink_ customHtml att con =
--     view customHtml { empty | attributes = att, label = con, isInline = True }


{-| Accepts a Renderer and a Link and the current region and url.
-}
view :
    OrHeader region
    -> State
    -> Templates html
    -> Style html
    -> Link
    -> Get (OrHeader region) html
view region state elements { isInline, label } link =
    Get.singleton
        (if isInline then
            region

         else
            Header
        )
    <|
        case link of
            Toggle _ flag ->
                elements.switch
                    { href = toString link
                    , label = label
                    , isChecked = stateHasFlag flag state
                    }

            Filter assignment ->
                elements.search
                    { assignment = assignment
                    , label = label
                    , isCurrent = stateHasAssignment assignment state
                    }

            _ ->
                elements.link
                    { href = toString link
                    , label = label
                    , isCurrent = getDestination link == Just (getStateLocation state)
                    }


{-| `isInline` : Draw the Link itself at the current region, otherwise in the `Header` region.
-}
type alias Style html =
    { isInline : Bool
    , label : html
    }


{-| -}
mapStyle : (html -> html2) -> Style html -> Style html2
mapStyle fu linkStyle =
    { isInline = linkStyle.isInline
    , label = fu linkStyle.label
    }


{-| Use this if you are working with `Styled Html`, `elm-ui` and friends

(Future: Add radio for tabs and exclusive ViewMode toggles)

-}
type alias Templates html =
    { link :
        { href : String, label : html, isCurrent : Bool }
        -> html
    , switch :
        { href : String, label : html, isChecked : Bool }
        -> html
    , search :
        { assignment : Assignment, label : html, isCurrent : Bool }
        -> html
    }



---- Codecs ----


{-|

    buildLink "Path" (Just "Fragment") Nothing Nothing Nothing True
        -> goTo (Just "Path", Just "Fragment")

    buildLink "Path" Nothing Nothing Nothing Nothing True
        -> goTo (Just "Path", Nothing)

Note that any flag will de-activate path and fragment.

-}
buildLink : Path -> Fragment -> Maybe String -> Maybe Flag -> Maybe String -> Bool -> Link
buildLink pathString maybeFragment reroute maybeFlag errorMessage isAbsolute =
    let
        myPath : Maybe Path
        myPath =
            parsePath pathString
    in
    case ( errorMessage, reroute, maybeFlag ) of
        ( Just e, _, _ ) ->
            err e

        ( _, Just here, _ ) ->
            Bounce { isAbsolute = isAbsolute } { there = ( myPath, maybeFragment ), here = queryParseLocation here }

        ( _, _, Just flag ) ->
            Toggle { isAbsolute = isAbsolute } flag

        _ ->
            GoTo ( myPath, maybeFragment )


codecs : List (Codec Link)
codecs =
    let
        getAbsoluteFlag : Url.Codec.CodecInProgress Link (Bool -> parseResult) -> Url.Codec.CodecInProgress Link parseResult
        getAbsoluteFlag =
            Url.Codec.queryFlag "!" <|
                \l ->
                    case l of
                        Bounce { isAbsolute } _ ->
                            isAbsolute

                        Toggle { isAbsolute } _ ->
                            isAbsolute

                        _ ->
                            False

        getError : Url.Codec.CodecInProgress Link (Maybe String -> Bool -> parseResult) -> Url.Codec.CodecInProgress Link (Bool -> parseResult)
        getError =
            Url.Codec.queryString "error"
                (\l ->
                    case l of
                        ErrorMessage e ->
                            Just e

                        _ ->
                            Nothing
                )

        getReroute : Url.Codec.CodecInProgress Link (Maybe String -> Maybe String -> Maybe String -> Bool -> parseResult) -> Url.Codec.CodecInProgress Link (Maybe String -> Maybe String -> Bool -> parseResult)
        getReroute =
            Url.Codec.queryString "reroute"
                (\l ->
                    case l of
                        Bounce _ { here } ->
                            Just (querySerialiseLocation here)

                        _ ->
                            Nothing
                )

        getToggle : Url.Codec.CodecInProgress Link (Maybe String -> Maybe String -> Bool -> parseResult) -> Url.Codec.CodecInProgress Link (Maybe String -> Bool -> parseResult)
        getToggle =
            Url.Codec.queryString "toggle"
                (\l ->
                    case l of
                        Toggle _ flag ->
                            Just flag

                        _ ->
                            Nothing
                )
    in
    [ Url.Codec.succeed buildLink
        (\_ -> True)
        |> Url.Codec.string getPath
    , Url.Codec.succeed (buildLink "")
        (\_ -> True)
    ]
        |> List.map
            (Url.Codec.fragment getFragment
                >> getReroute
                >> getToggle
                >> getError
                >> getAbsoluteFlag
            )



---- Apply ----


{-| -}
applyTransition : State -> Link -> State
applyTransition current link =
    toTransition link current


toTransition : Link -> Transition
toTransition link =
    case link of
        GoTo destination ->
            stateSetLocation destination

        Bounce { isAbsolute } { there, here } ->
            \state ->
                if
                    isAbsolute
                        || (there == getStateLocation state)
                        || (Tuple.first there == Nothing && Tuple.second there == Tuple.second (getStateLocation state))
                then
                    stateSetLocation here state

                else
                    stateSetLocation there state

        Toggle { isAbsolute } f ->
            stateRemoveAssignments [ "toggle", "reroute" ]
                >> (if isAbsolute then
                        stateTurnOnFlag f

                    else
                        stateToggleFlag f
                   )

        Filter assignment ->
            stateAddAssignment assignment

        ErrorMessage e ->
            stateAddAssignment ( "errorMessage", e )



---- InternalHelpers ----


{-| Use in encoding a query assignment for contextual linking.

    querySerialiseLocation (Just "", Nothing)
        --> "/"

    querySerialiseLocation (Nothing, Nothing)        -- Perhaps we need to prevent this case
        --> ""

    querySerialiseLocation (Nothing, Just "f")
        --> "~f"

    querySerialiseLocation (Nothing, Just "~")
        --> "~~"

    querySerialiseLocation (Just "~", Nothing)
        --> "%7E"

    querySerialiseLocation (Just "p", Nothing)
        --> "p"

    querySerialiseLocation (Just "p", Just "f")
        --> "p~f"

-}
querySerialiseLocation : ( Maybe Path, Fragment ) -> String
querySerialiseLocation ( maybePath, fragment ) =
    querySerialisePath maybePath ++ Maybe.unwrap "" (String.cons '~') fragment


serialisePath : Maybe Path -> String
serialisePath maybePath_ =
    case maybePath_ of
        Just nonEmpty ->
            "/" ++ String.replace "~" "%7E" nonEmpty

        Nothing ->
            ""


querySerialisePath : Maybe Path -> String
querySerialisePath maybePath_ =
    case maybePath_ of
        Just "" ->
            "/"

        Just nonEmpty ->
            String.replace "~" "%7E" nonEmpty

        Nothing ->
            ""


{-|

    queryParseLocation ""
        --> (Nothing, Nothing)

    queryParseLocation "/"
        --> (Just "", Nothing)

    queryParseLocation "~"
        --> (Nothing, Nothing)    -- Path may need to be coerced to ""

    queryParseLocation "~f"
        --> (Nothing, Just "f")

    queryParseLocation "/~f"
        --> (Just "", Just "f")

    queryParseLocation "~~"
        --> (Nothing, Just "~")

    queryParseLocation "p"
        --> (Just "p", Nothing)

    queryParseLocation "p~"       -- Will be sanitised to "p"
        --> (Just "p", Nothing)

    queryParseLocation "p~f"
        --> (Just "p", Just "f")

-}
queryParseLocation : String -> ( Maybe Path, Fragment )
queryParseLocation str =
    case String.split "~" str of
        [] ->
            ( Nothing, Nothing )

        [ path ] ->
            ( upTo "#" path
                |> Maybe.andThen parsePath
            , Nothing
            )

        path :: fragment ->
            ( parsePath path, upTo "#" <| String.join "~" fragment )


serializeQuery : Query -> Maybe String
serializeQuery query =
    Set.toList query.flags
        ++ List.map (\( k, v ) -> k ++ "=" ++ v) query.assignments
        |> String.join "&"
        |> String.nonEmpty


parseQueryString : Maybe String -> Query
parseQueryString =
    Maybe.withDefault ""
        >> String.split "&"
        >> List.foldr
            (\entry query ->
                case String.split "=" entry of
                    [] ->
                        query

                    [ "" ] ->
                        query

                    [ flag ] ->
                        { query | flags = Set.insert flag query.flags }

                    ass :: ignment ->
                        { query | assignments = ( ass, String.join "=" ignment ) :: query.assignments }
            )
            { flags = Set.empty, assignments = [] }


parsePath : String -> Maybe Path
parsePath str =
    case str of
        "" ->
            Nothing

        nonEmpty ->
            (stripPrefix "/" >> String.replace "%7E" "~" >> Just) nonEmpty


stripPrefix : String -> String -> String
stripPrefix prefix str =
    case String.split prefix str of
        _ :: r :: est ->
            String.join prefix (r :: est)

        _ ->
            str


{-|

    upTo "?" ""
        --> Nothing

    upTo "?" "!"
        --> Just "!"

    upTo "?" "!?1234"
        --> Just "!"

-}
upTo : String -> String -> Maybe String
upTo searchString =
    String.split searchString
        >> List.head
        >> Maybe.andThen String.nonEmpty
