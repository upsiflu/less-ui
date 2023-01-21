module Ui.Link exposing
    ( goTo, bounce, toggle
    , Link(..), fromUrl
    , relative
    , view, Renderer, preset, Position(..)
    , toUrlString, toStateTransition
    , a
    , queryParseLocation, querySerialiseLocation, upTo
    )

{-| Generate relative [`UrlRequest`s](../../../elm/browser/latest/Browser#UrlRequest) on click

  - [Jump Navigation (goTo)](#goTo)
      - [With two consecutive navigation steps (bounce)](#bounce)
  - [Progressive Disclosure (toggle)](#toggle)
      - unique at a given Ui node (tab) [ToDo]
      - unique in the browser tab (drowdown, dialog) [ToDo]

@docs goTo, bounce, toggle

@docs Link, fromUrl


# Map

@docs relative


# View

@docs view, Renderer, preset, Position


# Deconstruct

@docs toUrlString, toStateTransition


# Convenience

@docs a


# Advanced

The following functions are mostly here for testing

@docs queryParseLocation, querySerialiseLocation, upTo

-}

import Html exposing (Html)
import Html.Attributes exposing (..)
import String.Extra as String
import Ui exposing (Ui)
import Ui.Layout.Aspect as Aspect exposing (Aspect)
import Ui.Layout.ViewModel exposing (Foliage)
import Ui.State exposing (Flag, Fragment, Path, State)
import Url exposing (Url)
import Url.Codec exposing (Codec, ParseError(..))


{-| Encodes a transformation of [the Ui State](Ui.State)

Note concerning `Bounce`:

    url
    path#fragment        in assignment     Path, Fragment

    /a#b                 a~b               "a", Just "b"
    /a                   a                 "a", Nothing
    /#b                  ~b                "",  Just "b"
    /                                      "",  Nothing

    BUT what we need is a structure that can express
    - Path and Fragment
    - Empty Path and Fragment
    - Path but no Fragment
    - Empty Path but no Fragment
    - No Path but Fragment

    ILLEGAL: No Path and No Fragment.
    The easiest way would be to interpret Nothing in Path as "no path" unless fragment is Nothing,
    in which case Path==Nothing is interpreted as Just ""

Note that in `Bounce` and `GoTo`, (Nothing, Nothing) is coerced into (Just "", Nothing)

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
    { global : List (Html.Attribute Never) -> List (Html Never) -> Renderer
    , inline : List (Html.Attribute Never) -> List (Html Never) -> Renderer
    }
preset =
    { global = \att con -> { empty | attributes = att, contents = con, position = Global }
    , inline = \att con -> { empty | attributes = att, contents = con, position = Inline }
    }


empty : Renderer
empty =
    { attributes = []
    , contents = []
    , occlusions = Aspect.all
    , position = Global
    }


{-| How to present a Link
-}
type alias Renderer =
    { attributes : List (Html.Attribute Never)
    , contents : List (Html Never)
    , occlusions : List Aspect
    , position : Position
    }


{-| a bit like `Html.a`
-}
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
view :
    Renderer
    -> Link
    -> Ui (Html msg)
view config link =
    Ui.custom <|
        \( aspect, url ) ->
            let
                linkWithAttributes : List (Html.Attribute Never) -> Foliage (Html msg)
                linkWithAttributes additionalAttributes =
                    [ ( toId link, a link (config.attributes ++ additionalAttributes) config.contents ) ]

                where_ : Maybe Aspect
                where_ =
                    case config.position of
                        Inline ->
                            Just aspect

                        Global ->
                            Nothing
            in
            case link of
                Toggle _ flag ->
                    let
                        ( isChecked, mask ) =
                            if Ui.State.hasFlag flag url then
                                ( "true", [] )

                            else
                                ( "false", config.occlusions )
                    in
                    { occlude = mask
                    , appendWhere = where_
                    , appendWhat =
                        linkWithAttributes
                            [ attribute "role" "switch"
                            , attribute "aria-checked" isChecked
                            ]
                    }

                _ ->
                    { occlude = []
                    , appendWhere = where_
                    , appendWhat = linkWithAttributes []
                    }


{-| Define whether the link anchor
appears as a global handle or in the
`Aspect` of the current Item
-}
type Position
    = Inline
    | Global


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


{-|

    Note that in Elm, an absolute Url path always starts with a slash.
    To check if the intended path is relative, we compare it with the previous path first.

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
fromUrl : Path -> Url -> Link
fromUrl currentPath url =
    case
        { url
            | path =
                if currentPath == url.path then
                    ""

                else
                    url.path
        }
            |> Url.Codec.parseUrl codecs
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
toStateTransition : Link -> State -> State
toStateTransition link =
    let
        getLocation : State -> ( Maybe String, Fragment )
        getLocation state =
            ( Ui.State.getPath state |> String.nonEmpty, Ui.State.getFragment state )

        setLocation : ( Maybe Path, Fragment ) -> State -> State
        setLocation destination =
            case destination of
                ( Just path, fragment ) ->
                    Ui.State.setPath path >> Ui.State.setFragment fragment

                ( Nothing, fragment ) ->
                    Ui.State.setFragment fragment
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
            Ui.State.removeAssignments [ "toggle", "reroute" ]
                >> (if isAbsolute then
                        Ui.State.turnOnFlag f

                    else
                        Ui.State.toggleFlag f
                   )

        ErrorMessage err ->
            Ui.State.addAssignment "errorMessage" err


{-| -}
toHref : Link -> Html.Attribute Never
toHref =
    toUrlString
        >> Html.Attributes.href


{-| Try to create an UrlString

    --Bounce

    Bounce { isAbsolute = False }
        { there = (Just "there", Just "f1")
        , here = (Just "here", Just "f2")
        }
        |> toUrlString
        --> "/there?reroute=here~f2#f1"

    Bounce { isAbsolute = False }
        { there = (Just "there", Just "f1")
        , here = (Just "", Just "f2")
        }
        |> toUrlString
        --> "/there?reroute=/~f2#f1"

    Bounce { isAbsolute = False }
        { there = (Just "there", Just "f1")
        , here = (Just "", Nothing)
        }
        |> toUrlString
        --> "/there?reroute=/#f1"

    Bounce { isAbsolute = False }
        { there = (Just "", Just "f1")
        , here = (Just "here", Just "f2")
        }
        |> toUrlString
        --> "/?reroute=here~f2#f1"

    Bounce { isAbsolute = False }
        { there = (Just "", Nothing)
        , here = (Just "here", Just "f2")
        }
        |> toUrlString
        --> "/?reroute=here~f2"

    Bounce { isAbsolute = False }
        { there = (Nothing, Just "f1")
        , here = (Just "here", Just "f2")
        }
        |> toUrlString
        --> "?reroute=here~f2#f1"

    Bounce { isAbsolute = False }
        { there = (Nothing, Nothing)
        , here = (Just "here", Just "f2")
        }
        |> toUrlString
        --> "?reroute=here~f2"


    --Toggle

    Toggle {isAbsolute = False} "flag"
        |> toUrlString
        --> "?toggle=flag"

    Toggle {isAbsolute = True} "flag"
        |> toUrlString
        --> "?toggle=flag&!"


    --GoTo

    GoTo (Just "path", Just "fragment")
        |> toUrlString
        --> "/path#fragment"

    GoTo (Just "path", Nothing)
        |> toUrlString
        --> "/path"

    GoTo (Nothing, Just "fragment")
        |> toUrlString
        --> "#fragment"

    GoTo (Nothing, Nothing)
        |> toUrlString
        --> ""

    GoTo (Nothing, Just "")
        |> toUrlString
        --> "#"

-}
toUrlString : Link -> String
toUrlString =
    Url.Codec.toString codecs
        >> Maybe.andThen Url.percentDecode
        >> Maybe.withDefault "?errorMessage=Error converting link to string"


{-| -}
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


{-| Use in encoding a query assignment for contextual linking

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
    querySerialisePath maybePath ++ (Maybe.map (String.cons '~') fragment |> Maybe.withDefault "")


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
            ( Maybe.andThen parsePath (upTo "#" path), Nothing )

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
