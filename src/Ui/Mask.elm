module Ui.Mask exposing
    ( Mask
    , transparent, opaque
    , occlude, occludeList, superimpose
    , concat
    , mapSecond
    )

{-|

@docs Mask


# Create

@docs transparent, opaque
@docs occlude, occludeList, superimpose


# Compose

_Note that Masks are functions so you can compose them easily:_

`mask0 >> mask1` : combine two masks (occlusions accumulate, of course)

`mask0 get0` : apply a mask to a `get`

@docs concat


# Modify

@docs mapSecond

-}

import Bool.Extra as Bool
import Ui.Get as Get exposing (Get)
import Ui.Layout.Aspect exposing (Aspect)


{-| -}
type alias Mask a =
    Get a -> Get a



---- CREATE ----


{-| `transparent = identity`
-}
transparent : Mask a
transparent =
    identity


{-| -}
opaque : Mask a
opaque =
    \_ _ -> Nothing


{-| -}
occlude : Aspect -> Mask a
occlude a fu =
    (/=) a
        >> Bool.ifElse Nothing (fu a)


{-| `superimpose = Get.insert`
-}
superimpose : Aspect -> a -> Mask a
superimpose =
    Get.insert


{-| Occlude a list of aspects
-}
occludeList : List Aspect -> Mask a
occludeList =
    List.foldl cons transparent



---- COMPOSE ----


{-| Layer masks to let only the intersection through
-}
and : Mask a -> Mask a -> Mask a
and =
    (>>)


{-| occlude another aspect
-}
cons : Aspect -> Mask a -> Mask a
cons =
    occlude
        >> and


{-| combine a list of masks to a single masks. Nothing if list is empty.
-}
concat : List (Mask a) -> Mask a
concat =
    List.foldl append transparent


{-| Layer masks to let only the intersection through
-}
append : Mask a -> Mask a -> Mask a
append =
    and


{-| -}
mapSecond : (a -> b) -> Mask a -> Get a -> Get b
mapSecond =
    Get.map >> (<<)
