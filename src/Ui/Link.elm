module Ui.Link exposing (Link(..), Flag, Path, update, view)

{-| Generate relative [`UrlRequest`s](../../../elm/browser/latest/Browser#UrlRequest) on click

A [restrictive `Application`](Ui.Application) stores the local state in the Url (which then acts as the single source of truth).
The result of clicking on a link depends on the current state.

@docs Link, Flag, Path, update, view

-}

import Bool.Extra as Bool
import Html exposing (Html)
import Html.Attributes exposing (..)
import List.Extra as List
import Maybe.Extra as Maybe
import Ui.Layout.Aspect exposing (Aspect)
import Ui.Mask as Mask exposing (Mask)
import Url exposing (Url)
import Url.Codec exposing (Codec)


{-| Relative reference.


### SetPath

Change the path in the Url for the next `view`

    a   -> b -> b
    a?f -> b -> b?f

```css
a:active, a:link:active, a:visited:active {}
```


### Bounce

If you are `there`, go `here`, else go `there`.
Use for expanding/collapsing nodes, or for tree-shaped scenes in general (in which case, "here" is the parent path).

    a -> b?reroute=c -> b
    c -> b?reroute=c -> b
    b -> b?reroute=c -> c

`Bounce {"there" "here"}` yields an `Internal` request of `there?reroute=here`

```css
a:active, a:link:active, a:visited:active {}
a.bounce {}
```


### Toggle

Turn a flag on or off

    a?f -> ?toggle=f -> a

```css
a[role="switch"]:aria-checked {}
```

_Note:_ If you want to create a global Toggle, use Ui.toggle instead.

-}
type Link
    = SetPath Path
    | Bounce { there : Path, here : Path }
    | Toggle Flag


{-| [`State`](Ui.State) reflects the cumulative state of all [`Handle`](#Handle)s.
Turning off a `Flag` renders invisible the corresponding [`Control`](Ui.Layout.Aspect) with its descendants, as well as
one-layer deep nested [`Control`s](Ui.Layout.Aspect) with their descendants.

The pattern is **progressive disclosure**.

-}
type alias Flag =
    String


{-| Paths may represent an **editing-cursor position** or **viewport**. This is up to the app to define for now.
-}
type alias Path =
    String



---- Query ----


{-| -}
hasFlag : Flag -> Url -> Bool
hasFlag flag =
    getRoute
        >> .flags
        >> List.member flag



---- View ----


{-| -}
view : Url -> Link -> ( List (Html.Attribute Never) -> List (Html Never) -> Html Never, List Aspect -> Mask ui )
view url link =
    let
        ( attributes, mask ) =
            case link of
                Toggle flag ->
                    ( [ attribute "role" "switch"
                      , attribute "aria-checked" <| Bool.ifElse "true" "false" (hasFlag flag url)
                      ]
                    , if hasFlag flag url then
                        \_ -> Mask.transparent

                      else
                        Mask.occludeList
                    )

                _ ->
                    ( [], \_ -> Mask.transparent )

        href : List (Html.Attribute msg)
        href =
            Url.Codec.toString [ codec ] link
                |> Maybe.map Html.Attributes.href
                |> Maybe.toList
    in
    ( (++) (href ++ attributes)
        >> Html.a
    , mask
    )


{-| The Route represents both `href`s and `Url`s.

**State**: `path` (editing cursor and viewport), `flags` (progressive disclosure)

**Transition**: `reroute` (change path if it's already active), `toggle` (toggle Flag)

-}
type alias Route =
    { path : Path
    , flags : List Flag
    }


codec : Codec Link
codec =
    Url.Codec.succeed
        (\p r t ->
            case ( r, t ) of
                ( _, Just flag ) ->
                    Toggle flag

                ( Just there, _ ) ->
                    Bounce { there = there, here = p }

                ( Nothing, _ ) ->
                    SetPath p
        )
        (\_ -> True)
        |> Url.Codec.string
            (\m ->
                case m of
                    Bounce properties ->
                        Just properties.there

                    _ ->
                        Nothing
            )
        |> Url.Codec.queryString "reroute"
            (\m ->
                case m of
                    Bounce properties ->
                        Just properties.here

                    _ ->
                        Nothing
            )
        |> Url.Codec.queryString "toggle"
            (\m ->
                case m of
                    Toggle flag ->
                        Just flag

                    _ ->
                        Nothing
            )


route : Codec Route
route =
    Url.Codec.succeed Route (always True)
        |> Url.Codec.string (.path >> Just)
        |> Url.Codec.allQueryFlags .flags


getMsg : Url -> Link
getMsg =
    Url.Codec.parseUrl [ codec ]
        >> Result.withDefault (SetPath "")


getRoute : Url -> Route
getRoute =
    Url.Codec.parseUrl [ route ]
        >> Result.withDefault
            { path = ""
            , flags = []
            }


{-| Attention: Fails silently!
-}
modifyRoute : (Route -> Route) -> Url -> Url
modifyRoute fu previousUrl =
    replaceRoute
        (getRoute previousUrl |> fu)
        previousUrl


{-| Attention: Fails silently!
-}
replaceRoute : Route -> Url -> Url
replaceRoute r previousUrl =
    Url.Codec.toString [ route ] r
        |> Maybe.andThen Url.fromString
        |> Maybe.withDefault previousUrl



---- Update ----


{-| -}
update : Url -> Url -> Url
update receivedUrl =
    let
        receivedMsg : Link
        receivedMsg =
            getMsg receivedUrl

        toggleFlag : Flag -> List Flag -> List Flag
        toggleFlag f originalList =
            if List.member f originalList then
                List.remove f originalList

            else
                f :: originalList
    in
    modifyRoute <|
        \originalRoute ->
            case receivedMsg of
                SetPath p ->
                    { originalRoute | path = p }

                Bounce { there, here } ->
                    { originalRoute
                        | path =
                            if originalRoute.path == there then
                                here

                            else
                                there
                    }

                Toggle f ->
                    { originalRoute | flags = toggleFlag f originalRoute.flags }
