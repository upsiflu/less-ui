module Ui.Mask exposing
    ( Mask
    , transparent, opaque
    , occlude, occludeList, superimpose
    , concat, and
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

@docs concat, and


# Modify

@docs mapSecond

-}

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


{-| `superimpose = Get.remove`
-}
occlude : Aspect -> Mask a
occlude =
    Get.remove


{-| `superimpose = Get.insert`
-}
superimpose : Aspect -> a -> Mask a
superimpose =
    Get.insert


{-| Occlude a list of aspects

    import Ui.Layout.Aspect exposing (Aspect(..))
    import Ui.Get as Get exposing (get)

    Get.full ()
        |> occludeList [Control]
        |> get Control
        --> Nothing

    Get.full ()
        |> occludeList [Control, Scene]
        |> get Info
        --> Just ()

-}
occludeList : List Aspect -> Mask a
occludeList =
    List.foldl andOcclude transparent



---- COMPOSE ----


{-| Add occlusions

    import Ui.Get exposing (Get)
    import Ui.Layout.Aspect exposing (Aspect(..))

    mask : Mask Bool
    mask = occlude Scene |> and (occlude Info)

    get : Get Bool
    get = mask (Ui.Get.full True)

    get Scene --> Nothing
    get Info --> Nothing
    get Control --> Just True

-}
and : Mask a -> Mask a -> Mask a
and =
    (>>)


{-| Occlude one more aspect
-}
andOcclude : Aspect -> Mask a -> Mask a
andOcclude =
    occlude
        >> and


{-| combine a list of masks to a single masks. Nothing if list is empty.

    import Ui.Get
    import Ui.Layout.Aspect exposing (Aspect(..))

    Ui.Get.fromList [(Scene, "Scene"), (Control, "Control")]
        |> concat [occlude Scene, occlude Info]
        |> Ui.Get.values [Scene, Control, Info]
        --> ["Control"]

-}
concat : List (Mask a) -> Mask a
concat =
    List.foldl and transparent


{-| -}
mapSecond : (a -> b) -> Mask a -> Get a -> Get b
mapSecond =
    Get.map >> (<<)
