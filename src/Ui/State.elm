module Ui.State exposing
    ( State
    , fromUrl
    , evaluateQueryAssignments
    )

{-| We use the Url query to keep track of the Ui state. This makes sharing a Ui state as easy as copying the Url.

@docs State


# Create

@docs fromUrl


# Helper

@docs evaluateQueryAssignments

-}

import List.Extra as List
import Url exposing (Url)


{-| -}
type alias State =
    List String


{-| Calculate the new state according to query flags and assignments
-}
fromUrl : Url -> State
fromUrl url =
    []


{-| -}
evaluateQueryAssignments : Url -> Url
evaluateQueryAssignments url =
    url


{-| -}
toggleFlag : String -> State -> State
toggleFlag flag flags =
    flags
        |> (if List.member flag flags then
                List.remove flag

            else
                (::) flag
           )
