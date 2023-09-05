module Restrictive.State exposing
    ( State, Path, Flag, Assignment, Fragment, Query
    , fromString, err
    , next
    , setPath, setFragment
    , UrlCmd(..), integrateAssignment, addAssignment, removeAssignments, toggleFlag, turnOnFlag
    , hasFlag, linkData, linkStatus
    , getLocation, getFragment, getPath
    , toString
    , goTo, bounce, toggle, filter
    , Link, getLink
    , Templates, LinkStyle, LinkData(..)
    , relative
    , mapLinkStyle
    , Msg(..)
    , view
    , upTo, querySerialiseLocation, queryParseLocation, linkToString
    )

{-| We use the Url query to keep track of the Ui state. This makes sharing a Ui state as easy as copying the Url.

@docs State, Path, Flag, Assignment, Fragment, Query


# Create

@docs fromString, err


# Map

@docs next

@docs setPath, setFragment

@docs UrlCmd, integrateAssignment, addAssignment, removeAssignments, toggleFlag, turnOnFlag


# Query

@docs hasFlag, linkData, linkStatus


# Deconstruct

@docs getLocation, getFragment, getPath

@docs toString


# Create Links

Generate relative [`UrlRequest`s](../../../elm/browser/latest/Browser#UrlRequest) on click

  - Jump Navigation
      - [x]  [With a single target (goTo)](#goTo)
      - [x]  [With two back-and-forth targets (bounce)](#bounce)

  - Progressive Disclosure
      - [x]  [Orthogonal; any number can be active (toggle)](#toggle)
      - [ ] Exactly one active at a given Ui node (tab) ☞ [#8](https://github.com/upsiflu/restrictive/issues/8) ☞ [#2](https://github.com/upsiflu/restrictive/issues/2)
      - [ ] One or zero active in the browser tab (dropdown, dialog) ☞ [#8](https://github.com/upsiflu/restrictive/issues/8)

@docs goTo, bounce, toggle, filter

@docs Link, getLink

@docs Templates, LinkStyle, LinkData


# Map Links

@docs relative
@docs mapLinkStyle

@docs Msg


# View Links

@docs view


# Internals

Test these functions with `elm-verify-examples` (see the Readme)

@docs upTo, querySerialiseLocation, queryParseLocation, linkToString

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


{-| -}
type alias State =
    Url


{-| Handles your message type (`modelMsg`) as well as changes to the Ui state (Url).
-}
type Msg modelMsg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | AppMsg modelMsg
    | UrlCmds (List UrlCmd)


{-| -}
linkData : State -> Link -> Maybe LinkData
linkData url l =
    case l of
        GoTo _ ->
            Nothing

        Bounce _ _ ->
            Nothing

        Toggle _ _ ->
            Nothing

        Filter ( category, _ ) ->
            Maybe.map Filtered (getSearchTermOf category url)

        ErrorMessage _ ->
            Nothing


{-| -}
linkStatus : State -> Link -> Bool
linkStatus state l =
    case l of
        GoTo ( maybePath, _ ) ->
            maybePath == Just state.path

        Bounce _ { there } ->
            linkStatus state (GoTo there)

        Toggle _ f ->
            hasFlag f state

        Filter ( category, _ ) ->
            hasCategory category state

        ErrorMessage m ->
            hasError m state


{-| The data that a link emits in the context of a given Url.
-}
type LinkData
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


{-| Paths may represent an **editing-cursor position** or **viewport**.
This is up to the app to define for now.
-}
type alias Path =
    String


type alias Category =
    String


type alias SearchTerm =
    String


{-| **Viewmode**: Assign a value to a key. **Search**: Assign a searchTerm to a category.

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



---- Create ----


{-| Only use for testing!
-}
fromString : String -> Maybe State
fromString =
    Url.fromString



---- Map ----


{-| -}
setPath : Path -> State -> State
setPath path state =
    { state | path = "/" ++ path }


{-| -}
setFragment : Fragment -> State -> State
setFragment fragment state =
    { state | fragment = fragment }


{-| -}
setLocation : ( Maybe Path, Fragment ) -> State -> State
setLocation destination =
    case destination of
        ( Just path, fragment ) ->
            setPath path >> setFragment fragment

        ( Nothing, fragment ) ->
            setFragment fragment


{-| -}
turnOnFlag : Flag -> State -> State
turnOnFlag flag =
    if flag == "" then
        identity

    else
        mapQuery <| \q -> { q | flags = Set.insert flag q.flags }


{-|

    import Url exposing (Url)

    testQuery : String -> (Url -> Url) -> String
    testQuery query fu =
        "http://localhost/?" ++ query
            |> Url.fromString
            |> Maybe.andThen (fu >> .query)
            |> Maybe.withDefault "Url.fromString or .query failed"


    toggleFlag "g"
       |> testQuery "f&g&h&a=b&c=d=e"
                --> "f&h&a=b&c=d=e"


    toggleFlag "g"
       |> testQuery "f&h&a=b&c=d=e"
                --> "f&g&h&a=b&c=d=e"

-}
toggleFlag : Flag -> State -> State
toggleFlag flag =
    if flag == "" then
        identity

    else
        mapQuery <| \q -> { q | flags = Set.toggle flag q.flags }


{-| -}
integrateAssignment : UrlCmd -> State -> State
integrateAssignment urlCommand =
    case urlCommand of
        Set assignment ->
            replaceAssignment assignment

        Add assignment ->
            addAssignment assignment

        Clear k ->
            removeAssignments [ k ]


{-| -}
addAssignment : Assignment -> State -> State
addAssignment assignment =
    mapQuery <| \q -> { q | assignments = assignment :: q.assignments }


{-| -}
replaceAssignment : Assignment -> State -> State
replaceAssignment (( category, _ ) as assignment) =
    mapQuery <| \q -> { q | assignments = assignment :: List.filter (\( key, _ ) -> key /= category) q.assignments }


{-| -}
removeAssignments : List Category -> State -> State
removeAssignments categories =
    mapQuery <|
        \q ->
            { q
                | assignments =
                    List.filter
                        (Tuple.first >> (\key -> not (List.member key categories)))
                        q.assignments
            }


mapQuery : (Query -> Query) -> State -> State
mapQuery fu state =
    { state | query = (parseQueryString >> fu >> serializeQuery) state.query }


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



---- Query ----


{-|

    import Set
    import Url

    "http://a/?y"
        |> Url.fromString
        |> Maybe.map (hasFlag "z")
        --> Just False

    "http://a/?y&z"
        |> Url.fromString
        |> Maybe.map (hasFlag "z")
        --> Just True

-}
hasFlag : Flag -> State -> Bool
hasFlag flag =
    getFlags >> List.member flag


hasAssignment : Assignment -> State -> Bool
hasAssignment ( key, value ) =
    hasFlag (key ++ "=" ++ value)


hasError : String -> State -> Bool
hasError m =
    hasAssignment ( "e", m )


hasCategory : Category -> State -> Bool
hasCategory category =
    getFlags >> List.filterMap (String.split "=" >> List.head) >> List.member category



---- Deconstruct ----


{-| -}
toString : State -> String
toString =
    Url.toString


{-| -}
getFragment : State -> Fragment
getFragment =
    .fragment


{-| -}
getPath : State -> Path
getPath { path } =
    String.dropLeft 1 path


{-| -}
getLocation : State -> ( Maybe Path, Fragment )
getLocation url =
    ( String.nonEmpty (getPath url), getFragment url )


{-| -}
type alias Query =
    { flags : Set Flag, assignments : List Assignment }


getFlags : State -> List Flag
getFlags =
    .query >> parseQueryString >> .flags >> Set.toList


getAssignments : State -> List Assignment
getAssignments =
    .query >> parseQueryString >> .assignments


getLastAssignmentOf : Category -> State -> Maybe Assignment
getLastAssignmentOf category =
    getAssignments
        >> List.find (\( key, _ ) -> key == category)


getSearchTermOf : Category -> State -> Maybe SearchTerm
getSearchTermOf category =
    getLastAssignmentOf category
        >> Maybe.map Tuple.second



------------------
-----------------
----------------
--------------
-----------
-------


{-| Encodes an intended transition of [the Ui State](#State)
-}
type Link
    = GoTo ( Maybe Path, Fragment )
    | Bounce { isAbsolute : Bool } { there : ( Maybe Path, Fragment ), here : ( Maybe Path, Fragment ) }
    | Toggle { isAbsolute : Bool } Flag
    | Filter Assignment
    | ErrorMessage String


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


{-| Draw a link either inline or in the `Header` region
-}
type alias LinkStyle html =
    { isInline : Bool
    , label : html
    }


{-| -}
mapLinkStyle : (html -> html2) -> LinkStyle html -> LinkStyle html2
mapLinkStyle fu linkStyle =
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


{-| Accepts a Renderer and a Link and the current region and url
-}
view :
    OrHeader region
    -> Url
    -> Templates html
    -> LinkStyle html
    -> Link
    -> Get (OrHeader region) html
view region url elements { isInline, label } link =
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
                    { href = linkToString link
                    , label = label
                    , isChecked = hasFlag flag url
                    }

            Filter assignment ->
                elements.search
                    { assignment = assignment
                    , label = label
                    , isCurrent = hasAssignment assignment url
                    }

            _ ->
                elements.link
                    { href = linkToString link
                    , label = label
                    , isCurrent = unwrapDestination link == Just (getLocation url)
                    }


codecs : List (Codec Link)
codecs =
    let
        buildLink : Path -> Fragment -> Maybe String -> Maybe Flag -> Maybe String -> Bool -> Link
        buildLink pathString fragment reroute maybeFlag errorMessage isAbsolute =
            case ( parsePath pathString, reroute, maybeFlag ) of
                ( myPath, Just here, _ ) ->
                    Bounce { isAbsolute = isAbsolute } { there = ( myPath, fragment ), here = queryParseLocation here }

                ( _, _, Just flag ) ->
                    Toggle { isAbsolute = isAbsolute } flag

                ( myPath, _, Nothing ) ->
                    case ( errorMessage, fragment ) of
                        ( Just e, _ ) ->
                            err e

                        ( Nothing, Nothing ) ->
                            GoTo ( Just "", Nothing )

                        ( Nothing, orFragment ) ->
                            GoTo ( myPath, orFragment )

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
        |> Url.Codec.string destinationPath
    , Url.Codec.succeed (buildLink "")
        (\_ -> True)
    ]
        |> List.map
            (Url.Codec.fragment destinationFragment
                >> getReroute
                >> getToggle
                >> getError
                >> getAbsoluteFlag
            )


unwrapDestination : Link -> Maybe ( Maybe Path, Fragment )
unwrapDestination link =
    case link of
        GoTo location ->
            Just location

        Bounce _ { there } ->
            Just there

        _ ->
            Nothing


destinationPath : Link -> Maybe Path
destinationPath =
    unwrapDestination
        >> Maybe.andThen Tuple.first
        >> serialisePath
        >> Just


destinationFragment : Link -> Fragment
destinationFragment =
    unwrapDestination
        >> Maybe.andThen Tuple.second


{-| Generates a Link, given the current path.

Note that in Elm, an absolute Url path always starts with a slash.
To check if the path is intended to be relative, we compare it with the previous path first.
This is not absolutely necessary because the Elm runtime does the same check. However, it gives terser and more expressive Hrefs.

In the following tests, we assume a previous path of "/"

    import Url

    testFromUrl : String -> Link
    testFromUrl =
        (++) "http://localhost"
            >> Url.fromString
            >> Maybe.map (getLink "/")
            >> Maybe.withDefault (err "Url.fromString failed")



    --Bounce

    (testFromUrl) "/there?reroute=here~f2#f1"
        -->  bounce { there = (Just "there", Just "f1"), here = (Just "here", Just "f2") } |> relative


    (testFromUrl) "/there?reroute=/~f2#f1"
        -->  bounce { there = (Just "there", Just "f1"), here = (Just "", Just "f2") } |> relative

    (testFromUrl) "/there?reroute=~f2#f1"
        -->  bounce { there = (Just "there", Just "f1"), here = (Nothing, Just "f2") } |> relative


    (testFromUrl) "/there?reroute=#f1"
        -->  bounce  { there = (Just "there", Just "f1"), here = (Nothing, Nothing) } |> relative


    (testFromUrl) "/?reroute=here~f2#f1"
        --> bounce { there = (Nothing, Just "f1"), here = (Just "here", Just "f2") } |> relative


    (testFromUrl) "/?reroute=here~f2"
        --> bounce  { there = (Nothing, Nothing), here = (Just "here", Just "f2") } |> relative


    (testFromUrl) "/?reroute=~"
        --> bounce { there = (Nothing, Nothing), here = (Nothing, Nothing) } |> relative



    --Toggle

    (testFromUrl) "?toggle=flag"
        --> toggle "flag" |> relative

    (testFromUrl) "?toggle=flag&!"
        --> toggle "flag"


    --GoTo

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
getLink : State -> State -> Link
getLink new current =
    Url.Codec.parseUrl codecs
        { new
            | path =
                if current.path == new.path then
                    ""

                else
                    new.path
        }
        |> Result.extract parseErrorToLink


parseErrorToLink : ParseError -> Link
parseErrorToLink e =
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


{-| **Lifecycle of a Link**

1.  Link

2.  Dom `a` node with `href`

3a. Shared this `href`
-> Received new UrlString

-> 3aa. Init the app with that
-> 3ab. Update the app with that

3b. Clicked `Internal` (`href` -> `Url`) UrlRequest
-> Received new UrlString

-> Update the app with that

-}
next : Link -> State -> State
next link current =
    toStateTransition link current



---- Deconstruct ----


toStateTransition : Link -> State -> State
toStateTransition link =
    case link of
        GoTo destination ->
            setLocation destination

        Bounce { isAbsolute } { there, here } ->
            \state ->
                if
                    isAbsolute
                        || (there == getLocation state)
                        || (Tuple.first there == Nothing && Tuple.second there == Tuple.second (getLocation state))
                then
                    setLocation here state

                else
                    setLocation there state

        Toggle { isAbsolute } f ->
            removeAssignments [ "toggle", "reroute" ]
                >> (if isAbsolute then
                        turnOnFlag f

                    else
                        toggleFlag f
                   )

        Filter assignment ->
            addAssignment assignment

        ErrorMessage e ->
            addAssignment ( "errorMessage", e )


{-| Try to create an UrlString.

    --Bounce

    bounce
        { there = (Just "there", Just "f1")
        , here = (Just "here", Just "f2")
        }
        |> relative
        |> linkToString
        --> "/there?reroute=here~f2#f1"

    bounce
        { there = (Just "there", Just "f1")
        , here = (Just "", Just "f2")
        }
        |> relative
        |> linkToString
        --> "/there?reroute=/~f2#f1"

    bounce
        { there = (Just "there", Just "f1")
        , here = (Just "", Nothing)
        }
        |> relative
        |> linkToString
        --> "/there?reroute=/#f1"

    bounce
        { there = (Just "", Just "f1")
        , here = (Just "here", Just "f2")
        }
        |> relative
        |> linkToString
        --> "/?reroute=here~f2#f1"

    bounce
        { there = (Just "", Nothing)
        , here = (Just "here", Just "f2")
        }
        |> relative
        |> linkToString
        --> "/?reroute=here~f2"

    bounce
        { there = (Nothing, Just "f1")
        , here = (Just "here", Just "f2")
        }
        |> relative
        |> linkToString
        --> "?reroute=here~f2#f1"

    bounce
        { there = (Nothing, Nothing)
        , here = (Just "here", Just "f2")
        }
        |> relative
        |> linkToString
        --> "?reroute=here~f2"


    --Toggle

    toggle "flag"
        |> relative
        |> linkToString
        --> "?toggle=flag"

    toggle "flag"
        |> linkToString
        --> "?toggle=flag&!"


    --GoTo

    goTo (Just "path", Just "fragment")
        |> linkToString
        --> "/path#fragment"

    goTo (Just "path", Nothing)
        |> linkToString
        --> "/path"

    goTo (Nothing, Just "fragment")
        |> linkToString
        --> "#fragment"

    goTo (Nothing, Nothing)
        |> linkToString
        --> ""

    goTo (Nothing, Just "")
        |> linkToString
        --> "#"

-}
linkToString : Link -> String
linkToString =
    Url.Codec.toString codecs
        >> Maybe.andThen Url.percentDecode
        >> Maybe.withDefault "?errorMessage=Error converting link to string"


{-| By default, links render absolute, which means they don't respect
the current state. This is the desired behavior when sharing links.

For example, if you share a toggle, you want the receiver to
experience the "on" state; for a bounce, you want the "there" location,
regardless of the current state of their application.

When clicking a link in an application, a transition relative to the
current state is desired, so `Application.update` makes internal links relative.

-}
relative : Link -> Link
relative link =
    case link of
        Bounce _ location ->
            Bounce { isAbsolute = False } location

        Toggle _ flag ->
            Toggle { isAbsolute = False } flag

        _ ->
            link


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


parsePath : String -> Maybe Path
parsePath str =
    case String.replace "%7E" "~" str of
        "" ->
            Nothing

        nonEmpty ->
            Just (stripPrefix "/" nonEmpty)


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
