module Ui.State exposing
    ( State
    , fromUrl
    , update
    , init, toUrlString
    )

{-| We use the Url query to keep track of the Ui state. This makes sharing a Ui state as easy as copying the Url.

@docs State


# Create

@docs fromUrl


# Update

@docs update


# Helper

@docs evaluateQueryAssignments

-}

import List.Extra as List
import Ui.Link as Link exposing (Link(..))
import Url exposing (Url)


{-| -}
type alias State =
    Url


{-| -}
fromUrl : Url -> State
fromUrl =
    identity


{-| -}
toUrlString : State -> String
toUrlString =
    Url.toString


{-| -}
withError : String -> State -> State
withError str state =
    { state | query = Just (Maybe.withDefault "" state.query ++ "?error=" ++ str) }


{-|


## Motivation

The user toggles a Ui handle, for example the avatar, to open or close a menu.

(a)
They decide to share the link of the handle, so they right-click on the toggle and choose
'copy link'. Their friend opens the link and the handle is activated.

(b)
They copy the Url and paste it in another tab or browser or device.
The app loads and restores exactly the same Ui state.

In the case of (a), we share a href, which is a string.
The friend opens the link, and Elm turns it into the initial Url.
Now, `canonical.init` canonicalises the initial Url

**Lifecycle of a Link**

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
update : Link -> (State -> State)
update link state =
    case link of
        GoTo ( path, fragment ) ->
            { state | path = path, fragment = fragment }

        Bounce { isAbsolute } { there, here } ->
            let
                ( ( here_path, here_fragment ), ( there_path, there_fragment ) ) =
                    ( here, there )
            in
            if state.path == there_path || isAbsolute then
                { state
                    | path = here_path
                    , fragment = here_fragment
                }

            else
                { state
                    | path = there_path
                    , fragment = there_fragment
                }

        Toggle { isAbsolute } f ->
            { state
                | query =
                    state
                        |> Debug.log "update----current state:"
                        |> .query
                        |> Maybe.withDefault ""
                        |> String.split "&"
                        |> List.map (String.split "=")
                        |> List.foldr
                            (\param ( fAcc, aAcc ) ->
                                case param of
                                    [] ->
                                        ( fAcc, aAcc )

                                    [ "" ] ->
                                        ( fAcc, aAcc )

                                    [ flag ] ->
                                        ( flag :: fAcc, aAcc )

                                    ass :: signment ->
                                        if ass == "toggle" || ass == "reroute" then
                                            ( fAcc, aAcc )

                                        else
                                            ( fAcc, ( ass, String.join "=" signment ) :: aAcc )
                            )
                            ( [], [] )
                        |> Tuple.mapFirst
                            (\flags_ ->
                                if not (List.member f (Debug.log "current flags" flags_)) then
                                    f :: flags_

                                else if isAbsolute then
                                    flags_

                                else
                                    List.remove f flags_
                            )
                        |> Tuple.mapFirst (List.filter ((/=) ""))
                        |> Tuple.mapSecond
                            (List.map (\( k, v ) -> k ++ "=" ++ v))
                        |> Debug.log "update----(flags, assignments):"
                        |> (\( flags_, assignments_ ) ->
                                String.join "&" (flags_ ++ assignments_)
                           )
                        |> (\str ->
                                if str == "" then
                                    Nothing

                                else
                                    Just str
                           )
            }
                |> Debug.log "update----Result url:"

        ErrorMessage _ ->
            Link.toUrlString link
                |> Url.fromString
                |> Maybe.withDefault (withError "UrlToStringFailed" state)


init : State -> State
init state =
    { state | query = Nothing }
