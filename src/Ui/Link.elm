module Ui.Link exposing
    ( goTo, bounce, toggle
    , Link(..), fromUrl, Flag, Path, Fragment
    , relative
    , view, Renderer, preset, Position(..)
    , toUrlString, getFlags
    , a
    )

{-| Generate relative [`UrlRequest`s](../../../elm/browser/latest/Browser#UrlRequest) on click

  - [Jump Navigation (goTo)](#goTo)
      - [With two consecutive navigation steps (bounce)](#bounce)
  - [Progressive Disclosure (toggle)](#toggle)
      - unique at a given Ui node (tab) [ToDo]
      - unique in the browser tab (drowdown, dialog) [ToDo]

@docs goTo, bounce, toggle

@docs Link, fromUrl, Flag, Path, Fragment


# Map

@docs relative


# View

@docs view, Renderer, preset, Position


# Deconstruct

@docs toUrlString, getFlags


# Convenience

@docs a

-}

import Bool.Extra as Bool
import Html exposing (Html)
import Html.Attributes exposing (..)
import Maybe.Extra as Maybe
import String.Extra as String
import Ui exposing (Ui)
import Ui.Get as Get
import Ui.Layout.Aspect as Aspect exposing (Aspect)
import Ui.Layout.ViewModel as ViewModel
import Ui.Mask as Mask
import Url exposing (Url)
import Url.Codec exposing (Codec, ParseError(..))


{-| Encodes a transformation of [the Ui State](Ui.State)
-}
type Link
    = GoTo ( Path, Fragment )
    | Bounce { isAbsolute : Bool } { there : ( Path, Fragment ), here : ( Path, Fragment ) }
    | Toggle { isAbsolute : Bool } Flag
    | ErrorMessage String


{-| Change the path in the Url for the next `view`

    a   -> b -> b
    a?f -> b -> b?f

```css
a:active, a:link:active, a:visited:active {}
```

-}
goTo : ( Path, Fragment ) -> Link
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
bounce : { there : ( Path, Fragment ), here : ( Path, Fragment ) } -> Link
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


{-| Turning off a `Flag` renders invisible the corresponding [`Control`](Ui.Layout.Aspect) with its descendants, as well as
one-layer deep nested [`Control`s](Ui.Layout.Aspect) with their descendants.

The pattern is **progressive disclosure**.

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
the `:target` CSS pseudoclass; Wikipedia uses this to highlight the selected reference.
(from Wikipedia)"
-}
type alias Fragment =
    Maybe String



---- Query ----


{-| -}
hasFlag : Flag -> Url -> Bool
hasFlag flag =
    getFlags >> List.member flag



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
    , occlusions = [ Aspect.Control, Aspect.Scene, Aspect.Info ]
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
    Html.a (Maybe.cons (toHref link) attrs) contents
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
    -> Ui msg
view config link =
    Ui.custom <|
        \( aspect, url ) ->
            let
                transformViewModel : List (Html.Attribute Never) -> Ui.Custom msg
                transformViewModel additionalAttributes =
                    let
                        foliage : ViewModel.Foliage msg
                        foliage =
                            [ ( toId link, a link (config.attributes ++ additionalAttributes) config.contents ) ]
                    in
                    Ui.TransformViewModel <|
                        case config.position of
                            Inline ->
                                ViewModel.appendGet (Get.singleton aspect foliage)

                            Global ->
                                ViewModel.appendHandle foliage
            in
            case link of
                Toggle _ flag ->
                    let
                        ( isChecked, mask ) =
                            Bool.ifElse
                                ( "true", [] )
                                ( "false", [ Mask.occludeList config.occlusions |> Ui.MaskDescendents ] )
                                (hasFlag flag url)
                    in
                    transformViewModel
                        [ attribute "role" "switch"
                        , attribute "aria-checked" isChecked
                        ]
                        :: mask

                _ ->
                    [ transformViewModel [] ]


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
                    Bounce { isAbsolute = isAbsolute_ } { there = ( path_, Maybe.map (String.leftOf "?") fragment_ ), here = locationFromString here }

                Nothing ->
                    case toggle_ of
                        Just flag ->
                            Toggle { isAbsolute = isAbsolute_ } flag

                        Nothing ->
                            case errorMessage_ of
                                Just err ->
                                    ErrorMessage err

                                Nothing ->
                                    GoTo ( path_, fragment_ )

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

        getFragment : Url.Codec.CodecInProgress Link (Maybe String -> Maybe String -> Maybe String -> Maybe String -> Bool -> parseResult) -> Url.Codec.CodecInProgress Link (Maybe String -> Maybe String -> Maybe String -> Bool -> parseResult)
        getFragment =
            Url.Codec.fragment (getDestination >> Maybe.andThen Tuple.second)

        getPath : Url.Codec.CodecInProgress Link (String -> parseResult) -> Url.Codec.CodecInProgress Link parseResult
        getPath =
            Url.Codec.string (getDestination >> Maybe.map Tuple.first)

        getReroute : Url.Codec.CodecInProgress Link (Maybe String -> Maybe String -> Maybe String -> Bool -> parseResult) -> Url.Codec.CodecInProgress Link (Maybe String -> Maybe String -> Bool -> parseResult)
        getReroute =
            Url.Codec.queryString "reroute"
                (\l ->
                    case l of
                        Bounce _ properties ->
                            Just (locationToString properties.here)

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

        getDestination : Link -> Maybe ( Path, Fragment )
        getDestination l =
            case l of
                GoTo location ->
                    Just location

                Bounce _ { there } ->
                    Just there

                _ ->
                    Nothing
    in
    [ Url.Codec.succeed buildLink
        (\_ -> True)
        |> getPath
    , Url.Codec.succeed (buildLink "")
        (\_ -> True)
    ]
        |> List.map
            (getFragment
                >> getReroute
                >> getToggle
                >> getError
                >> getAbsoluteFlag
            )


{-|

    import Url

    testUrl : String -> Link
    testUrl =
        (++) "http://localhost/"
            >> Url.fromString
            >> Maybe.map fromUrl
            >> Maybe.withDefault (ErrorMessage "Url.fromString failed")


    --Bounce

    testUrl "there#f1?reroute=here~f2"
        --> Bounce { isAbsolute = False } { there = ("there", Just "f1"), here = ("here", Just "f2") }

    testUrl "#f1?reroute=here~f2"
        --> Bounce { isAbsolute = False } { there = ("", Just "f1"), here = ("here", Just "f2") }



    --Toggle

    testUrl "?toggle=flag"
        --> Toggle {isAbsolute = False} "flag"

    testUrl "?toggle=flag&!"
        --> Toggle {isAbsolute = True} "flag"


    --GoTo

    testUrl "path#fragment"
        --> GoTo ("path", Just "fragment")

    testUrl "path"
        --> GoTo ("path", Nothing)

    testUrl "path/"
        --> GoTo ("path", Nothing)

    testUrl "#fragment"
        --> GoTo ("", Just "fragment")

-}
fromUrl : Url -> Link
fromUrl =
    Url.Codec.parseUrl codecs
        >> (\result ->
                case result of
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
           )


flags : List (Codec (List String))
flags =
    [ Url.Codec.succeed identity (\_ -> True)
        |> Url.Codec.allQueryFlags identity
    , Url.Codec.succeed (\_ -> identity) (\_ -> True)
        |> Url.Codec.string (\_ -> Nothing)
        |> Url.Codec.allQueryFlags identity
    ]


{-| Attention: Fails silently!

    import Url

    testUrl : String -> List String
    testUrl =
        (++) "http://localhost/"
            >> Url.fromString
            >> Maybe.map getFlags
            >> Maybe.withDefault ["Url.fromString failed"]

    testUrl
        "" --> []

    testUrl
        "?a" --> ["a"]

    testUrl
        "?a&b" --> ["a", "b"]

    testUrl
        "path" --> []

    testUrl
        "path?a" --> ["a"]

    testUrl
        "path?a&b" --> ["a", "b"]

-}
getFlags : Url -> List Flag
getFlags =
    Url.Codec.parseUrl flags
        >> Result.withDefault [ "Error_in_GetFlags" ]


{-| -}
toHref : Link -> Maybe (Html.Attribute Never)
toHref =
    Url.Codec.toString codecs
        >> Maybe.map Html.Attributes.href


{-| Try to create an UrlString
-}
toUrlString : Link -> String
toUrlString =
    Url.Codec.toString codecs
        >> Maybe.withDefault "?error=Error converting link to string"


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
            locationToString location

        Bounce { isAbsolute } { there, here } ->
            locationToString there ++ "<>" ++ locationToString here ++ encodeState isAbsolute

        Toggle { isAbsolute } flag ->
            flag ++ encodeState isAbsolute

        ErrorMessage e ->
            e


{-| -}
locationToString : ( Path, Fragment ) -> String
locationToString ( path, fragment ) =
    path ++ (fragment |> Maybe.map (String.cons '~') |> Maybe.withDefault "")


{-| -}
locationFromString : String -> ( Path, Fragment )
locationFromString str =
    case String.split "~" str of
        [] ->
            ( "", Nothing )

        [ path ] ->
            ( path, Nothing )

        path :: fragment ->
            ( path, Just <| String.join "~" fragment )
