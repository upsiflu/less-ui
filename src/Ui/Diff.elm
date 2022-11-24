module Ui.Diff exposing
    ( Diff
    , apply
    )

{-| We use the Url query to keep track of the Ui state. This makes sharing a Ui state as easy as copying the Url.


# Create

@docs generate


# Helper

@docs evaluateQueryAssignments

-}

import Dict exposing (Dict)
import List.Extra as List
import Set
import Ui.State exposing (State)
import Url exposing (Url)


{-| -}
type alias Diff =
    Dict String String


{-| Generates a link that
-}
apply : Diff -> Url -> Url
apply diff url =
    url
