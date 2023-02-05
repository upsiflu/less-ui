module Restrictive.State exposing
    ( State, Path, Flag, Fragment, Query
    , init
    , map, setPath, setFragment
    , addAssignment, removeAssignments, toggleFlag, turnOnFlag
    , hasFlag
    , toUrlString
    , getFragment, getPath
    , headerLink, inlineLink
    , goTo, bounce, toggle
    , Link, getLink
    , relative
    , view
    , toStateTransition
    )

{-| We use the Url query to keep track of the Ui state. This makes sharing a Ui state as easy as copying the Url.

@docs State, Path, Flag, Fragment, Query


# Create

@docs init


# Map

@docs map, setPath, setFragment

@docs addAssignment, removeAssignments, toggleFlag, turnOnFlag


# Query

@docs hasFlag


# Deconstruct

@docs toUrlString
@docs getFragment, getPath


# Link

@docs headerLink, inlineLink

---


# Links are cool!

Generate relative [`UrlRequest`s](../../../elm/browser/latest/Browser#UrlRequest) on click

  - [Jump Navigation (goTo)](#goTo)
      - [With two consecutive navigation steps (bounce)](#bounce)
  - [Progressive Disclosure (toggle)](#toggle)
      - unique at a given Ui node (tab) [ToDo]
      - unique in the browser tab (drowdown, dialog) [ToDo]

@docs goTo, bounce, toggle

@docs Link, getLink


# Map

@docs relative


# View

@docs view


# Deconstruct

@docs toStateTransition


# Advanced

The following functions are mostly here for testing

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Maybe.Extra as Maybe
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout.Region exposing (OrAll(..), OrHeader(..))
import Restrictive.Mask as Mask exposing (Mask)
import Set exposing (Set)
import Set.Extra as Set
import String.Extra as String
import Url exposing (Url)
import Url.Codec exposing (Codec, ParseError(..))


{-| -}
type alias State =
    { current : Url, previous : Maybe Url }


{-| -}
map : (a -> b) -> { current : a, previous : Maybe a } -> { current : b, previous : Maybe b }
map fu state_ =
    { current = fu state_.current, previous = Maybe.map fu state_.previous }


{-| Turning off a `Flag` renders invisible the corresponding [Aspects](Ui.Layout.Aspect) in the corresponding [toggle Link](Ui.Link#toggle).

Assignments such as `?a=b` may represent currently active Tabs or a search string.

The patterns are **progressive disclosure** and **fulltext search**.

-}
type alias Flag =
    String


{-| Paths may represent an **editing-cursor position** or **viewport**. This is up to the app to define for now.
-}
type alias Path =
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
    Maybe String



---- Create ----


{-| -}
init : Url -> State
init state =
    { current = state, previous = Nothing }



---- Map ----


{-| -}
setPath : Path -> Url -> Url
setPath path state =
    { state | path = "/" ++ path }


{-| -}
setFragment : Fragment -> Url -> Url
setFragment fragment state =
    { state | fragment = fragment }


{-|

    import Url

    testQuery : (State -> State) -> String -> String
    testQuery fu =
        (++) "http://localhost/?"
            >> Url.fromString
            >> Maybe.andThen (fu >> .query)
            >> Maybe.withDefault "Url.fromString or .query failed"

    testQuery (turnOnFlag "g")
        "f&g&h&a=b&c=d=e"
    --> "f&g&h&a=b&c=d=e"

    testQuery (turnOnFlag "h")
        "a=b&c=d=e&f&g"
    --> "f&g&h&a=b&c=d=e"

    testQuery (turnOnFlag "")
        "f"
    --> "f"


    testQuery (turnOnFlag "h")
        ""
    --> "h"

    testQuery (turnOnFlag "")
        ""
    --> ""

-}
turnOnFlag : Flag -> Url -> Url
turnOnFlag flag =
    if flag == "" then
        identity

    else
        mapQuery <| \q -> { q | flags = Set.insert flag q.flags }


{-|

    import Url

    testQuery : (State -> State) -> String -> String
    testQuery fu =
        (++) "http://localhost/?"
            >> Url.fromString
            >> Maybe.andThen (fu >> .query)
            >> Maybe.withDefault "Url.fromString or .query failed"


    testQuery (toggleFlag "g")
        "f&g&h&a=b&c=d=e"
    --> "f&h&a=b&c=d=e"


    testQuery (toggleFlag "g")
        "f&h&a=b&c=d=e"
    --> "f&g&h&a=b&c=d=e"

-}
toggleFlag : Flag -> Url -> Url
toggleFlag flag =
    if flag == "" then
        identity

    else
        mapQuery <| \q -> { q | flags = Set.toggle flag q.flags }


{-|

    import Url

    testQuery : (State -> State) -> String -> String
    testQuery fu =
        (++) "http://./?"
            >> Url.fromString
            >> Maybe.andThen (fu >> .query)
            >> Maybe.withDefault "Url.fromString or .query failed"

    testQuery (addAssignment "c" "x")
        "f&g&h&a=b&c=d=e"
    --> "f&g&h&c=x&a=b&c=d=e"


    testQuery (addAssignment "" "x")
        ""
    --> "=x"

    testQuery (addAssignment "" "y")
        "=x"
    --> "=y&=x"

    testQuery (addAssignment "" "")
        "=y&=x"
    --> "=&=y&=x"

-}
addAssignment : String -> String -> Url -> Url
addAssignment key value =
    mapQuery <| \q -> { q | assignments = ( key, value ) :: q.assignments }


{-| -}
removeAssignments : List String -> Url -> Url
removeAssignments keys =
    mapQuery <|
        \q ->
            { q
                | assignments =
                    List.filter
                        (Tuple.first >> (\key -> not (List.member key keys)))
                        q.assignments
            }


mapQuery : (Query -> Query) -> Url -> Url
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
hasFlag : Flag -> Url -> Bool
hasFlag flag =
    getFlags >> List.member flag



---- Deconstruct ----


{-| -}
toUrlString : Url -> String
toUrlString =
    Url.toString


{-| -}
getFragment : Url -> Fragment
getFragment =
    .fragment


{-| -}
getPath : Url -> Path
getPath { path } =
    String.dropLeft 1 path


{-| -}
type alias Query =
    { flags : Set Flag, assignments : List ( String, String ) }


getFlags : Url -> List Flag
getFlags =
    .query >> parseQueryString >> .flags >> Set.toList



------------------
-----------------
----------------
--------------
-----------
-------


{-| Encodes an intended transition of [the Ui State](Ui.State).

Use the convenience functions in `Restrictive` to build

-}
type Link
    = GoTo ( Maybe Path, Fragment )
    | Bounce { isAbsolute : Bool } { there : ( Maybe Path, Fragment ), here : ( Maybe Path, Fragment ) }
    | Toggle { isAbsolute : Bool } Flag
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



---- View ----


{-| A familiar syntax, similar to how you compose Html:

    import Html

    let
        link =
            goTo ("myPath", Nothing)

        renderer =
            preset.global []
                [Html.text "go to my Path!"]

    in
    link |> view renderer

-}
preset :
    { global : List (Html.Attribute Never) -> List (Html Never) -> Renderer aspect
    , inline : List (Html.Attribute Never) -> List (Html Never) -> Renderer aspect
    , nav : List (Html.Attribute Never) -> List (Html Never) -> Renderer aspect
    , tab : List (Html.Attribute Never) -> List (Html Never) -> Renderer aspect
    }
preset =
    { global = \att con -> { empty | attributes = att, contents = con, position = always Header }
    , inline = \att con -> { empty | attributes = att, contents = con, position = identity }
    , nav = \att con -> { empty | attributes = Attr.type_ "radio" :: att, contents = con, element = Html.input, position = always Header }
    , tab = \att con -> { empty | attributes = Attr.type_ "radio" :: att, contents = con, element = Html.input, position = identity }
    }



-- NEED TO CHECK IF TRANSFORMATION CAN HAVE ALL!


empty : Renderer region
empty =
    { attributes = []
    , contents = []
    , element = Html.a
    , occlusions = All
    , position = always Header
    }


{-| How to present a Link
-}
type alias Renderer region =
    { attributes : List (Html.Attribute Never)
    , contents : List (Html Never)
    , element : List (Html.Attribute Never) -> List (Html Never) -> Html Never
    , occlusions : OrAll region
    , position : OrHeader region -> OrHeader region
    }


{-| simple inline link
-}
headerLink : Link -> (( OrHeader region, Url ) -> { linkHtml : Get (OrHeader region) (List ( String, Html msg )), occlude : Mask region a })
headerLink link =
    view (preset.global [] [ Html.text (toId link) ]) link


{-| simple inline link
-}
inlineLink : Link -> (( OrHeader region, Url ) -> { linkHtml : Get (OrHeader region) (List ( String, Html msg )), occlude : Mask region a })
inlineLink link =
    view (preset.inline [] [ Html.text (toId link) ]) link


{-| -}
a : Link -> List (Html.Attribute Never) -> List (Html Never) -> Html msg
a link attrs contents =
    Html.a (toHref link :: attrs) contents
        |> Html.map never


{-|

    import Html

    toggle "mainMenu"
        |> view
            { attributes = []
            , contents = [ Html.text "☰" ]
            , occlusions = []
            , position = Global
            }

-}
view : Renderer region -> Link -> ( OrHeader region, Url ) -> { linkHtml : Get (OrHeader region) (List ( String, Html msg )), occlude : Mask region a }
view config link =
    \( region, url ) ->
        let
            linkWithAttributes : List (Html.Attribute Never) -> List ( String, Html msg )
            linkWithAttributes additionalAttributes =
                [ ( toId link, a link (config.attributes ++ additionalAttributes) config.contents ) ]
        in
        case link of
            Toggle _ flag ->
                let
                    ( isChecked, mask ) =
                        if hasFlag flag url then
                            ( "true", identity )

                        else
                            ( "false", Mask.occludeOrAll config.occlusions )
                in
                { linkHtml =
                    linkWithAttributes
                        [ Attr.attribute "role" "switch"
                        , Attr.attribute "aria-checked" isChecked
                        ]
                        |> Get.singleton (config.position region)
                , occlude = mask
                }

            _ ->
                { linkHtml =
                    linkWithAttributes []
                        |> Get.singleton (config.position region)
                , occlude = identity
                }


codecs : List (Codec Link)
codecs =
    let
        buildLink : Path -> Maybe String -> Maybe String -> Maybe Flag -> Maybe String -> Bool -> Link
        buildLink path_ fragment_ reroute_ toggle_ errorMessage_ isAbsolute_ =
            case reroute_ of
                Just here ->
                    Bounce { isAbsolute = isAbsolute_ } { there = ( parsePath path_, fragment_ ), here = queryParseLocation here }

                Nothing ->
                    case toggle_ of
                        Just flag ->
                            Toggle { isAbsolute = isAbsolute_ } flag

                        Nothing ->
                            case errorMessage_ of
                                Just err ->
                                    ErrorMessage err

                                Nothing ->
                                    GoTo ( parsePath path_, fragment_ )

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

    testFromUrl :  String -> Link
    testFromUrl =
        (++) "http://localhost"
            >> Url.fromString
            >> Maybe.map (fromUrl "/")
            >> Maybe.withDefault (ErrorMessage "Url.fromString failed")


    --Bounce

    testFromUrl "/there?reroute=here~f2#f1"
        --> Bounce { isAbsolute = False } { there = (Just "there", Just "f1"), here = (Just "here", Just "f2") }

    testFromUrl "/there?reroute=/~f2#f1"
        --> Bounce { isAbsolute = False } { there = (Just "there", Just "f1"), here = (Just "", Just "f2") }

    testFromUrl "/there?reroute=~f2#f1"
        --> Bounce { isAbsolute = False } { there = (Just "there", Just "f1"), here = (Nothing, Just "f2") }

    testFromUrl "/there?reroute=#f1"
        --> Bounce { isAbsolute = False } { there = (Just "there", Just "f1"), here = (Nothing, Nothing) }

    testFromUrl "/?reroute=here~f2#f1"
        --> Bounce { isAbsolute = False } { there = (Nothing, Just "f1"), here = (Just "here", Just "f2") }

    testFromUrl "/?reroute=here~f2"
        --> Bounce { isAbsolute = False } { there = (Nothing, Nothing), here = (Just "here", Just "f2") }

    testFromUrl "/?reroute=~"
        --> Bounce { isAbsolute = False } { there = (Nothing, Nothing), here = (Nothing, Nothing) }


    --Toggle

    testFromUrl "?toggle=flag"
        --> Toggle {isAbsolute = False} "flag"

    testFromUrl "?toggle=flag&!"
        --> Toggle {isAbsolute = True} "flag"


    --GoTo

    testFromUrl "/path#fragment"
        --> GoTo (Just "path", Just "fragment")

    testFromUrl "/path"
        --> GoTo (Just "path", Nothing)

    testFromUrl "/path/"
        --> GoTo (Just "path", Nothing)

    testFromUrl "/#fragment"
        --> GoTo (Nothing, Just "fragment")


    testFromUrl "/#?fragment"
        --> GoTo (Nothing, Just "?fragment")

-}
getLink : Path -> Url -> Link
getLink currentPath url =
    case
        Url.Codec.parseUrl codecs
            { url
                | path =
                    if currentPath == url.path then
                        ""

                    else
                        url.path
            }
    of
        Ok link ->
            link

        Err e ->
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



---- Deconstruct ----


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
toStateTransition : Link -> Url -> Url
toStateTransition link =
    let
        getLocation : Url -> ( Maybe String, Fragment )
        getLocation url =
            ( String.nonEmpty (getPath url), getFragment url )

        setLocation : ( Maybe Path, Fragment ) -> Url -> Url
        setLocation destination =
            case destination of
                ( Just path, fragment ) ->
                    setPath path >> setFragment fragment

                ( Nothing, fragment ) ->
                    setFragment fragment
    in
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

        ErrorMessage err ->
            addAssignment "errorMessage" err


{-| -}
toHref : Link -> Html.Attribute Never
toHref =
    linkToString
        >> Attr.href


{-| Try to create an UrlString.

    --Bounce

    Bounce { isAbsolute = False }
        { there = (Just "there", Just "f1")
        , here = (Just "here", Just "f2")
        }
        |> linkToString
        --> "/there?reroute=here~f2#f1"

    Bounce { isAbsolute = False }
        { there = (Just "there", Just "f1")
        , here = (Just "", Just "f2")
        }
        |> linkToString
        --> "/there?reroute=/~f2#f1"

    Bounce { isAbsolute = False }
        { there = (Just "there", Just "f1")
        , here = (Just "", Nothing)
        }
        |> linkToString
        --> "/there?reroute=/#f1"

    Bounce { isAbsolute = False }
        { there = (Just "", Just "f1")
        , here = (Just "here", Just "f2")
        }
        |> linkToString
        --> "/?reroute=here~f2#f1"

    Bounce { isAbsolute = False }
        { there = (Just "", Nothing)
        , here = (Just "here", Just "f2")
        }
        |> linkToString
        --> "/?reroute=here~f2"

    Bounce { isAbsolute = False }
        { there = (Nothing, Just "f1")
        , here = (Just "here", Just "f2")
        }
        |> linkToString
        --> "?reroute=here~f2#f1"

    Bounce { isAbsolute = False }
        { there = (Nothing, Nothing)
        , here = (Just "here", Just "f2")
        }
        |> linkToString
        --> "?reroute=here~f2"


    --Toggle

    Toggle {isAbsolute = False} "flag"
        |> linkToString
        --> "?toggle=flag"

    Toggle {isAbsolute = True} "flag"
        |> linkToString
        --> "?toggle=flag&!"


    --GoTo

    GoTo (Just "path", Just "fragment")
        |> linkToString
        --> "/path#fragment"

    GoTo (Just "path", Nothing)
        |> linkToString
        --> "/path"

    GoTo (Nothing, Just "fragment")
        |> linkToString
        --> "#fragment"

    GoTo (Nothing, Nothing)
        |> linkToString
        --> ""

    GoTo (Nothing, Just "")
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


{-| -}
toId : Link -> String
toId link =
    let
        encodeState : Bool -> String
        encodeState isAbsolute =
            if isAbsolute then
                "!"

            else
                ""
    in
    case link of
        GoTo location ->
            querySerialiseLocation location

        Bounce { isAbsolute } { there, here } ->
            querySerialiseLocation there ++ "<>" ++ querySerialiseLocation here ++ encodeState isAbsolute

        Toggle { isAbsolute } flag ->
            flag ++ encodeState isAbsolute

        ErrorMessage e ->
            e


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
