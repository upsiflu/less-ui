module Restrictive.Mask exposing
    ( Mask
    , transparent, opaque
    , occlude, occludeList, occludeOrAll, superimpose
    , filter
    , concat, and
    , map, mapKey, mapSecond, invert
    , mask
    )

{-|

@docs Mask
@docs transparent, opaque
@docs occlude, occludeList, occludeOrAll, superimpose
@docs filter

_In addition, many function in `Get` return Masks. Of use may be [`Get.map`](Ui.Get#map)_


# Compose

_Note that Masks are functions so you can compose them easily:_

`mask0 >> mask1` : combine two masks (occlusions accumulate, of course)

`mask0 get0` : apply a mask to a `get`

@docs concat, and


# Modify

@docs map, mapKey, mapSecond, invert


# Apply

@docs mask

-}

import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout.Region exposing (OrAll(..))


{-| -}
type alias Mask key a =
    Get key a -> Get key a



---- CREATE ----


{-| -}
transparent : Mask key a
transparent =
    identity


{-| -}
opaque : Mask key a
opaque =
    \_ _ -> Nothing


{-| remove aspect
-}
occlude : key -> Mask key a
occlude =
    Get.remove


{-| insert (and overwrite) at aspect
-}
superimpose : key -> a -> Mask key a
superimpose =
    Get.insert


{-| Occlude a list of aspects

    import Ui.Layout.Aspect exposing (Aspect(..))
    import Ui.Get key as Get key exposing (get)

    Get.full ()
        |> occludeList [Control]
        |> get Control
        --> Nothing

    Get.full ()
        |> occludeList [Control, Scene]
        |> get Info
        --> Just ()

-}
occludeList : List key -> Mask key a
occludeList =
    List.foldl andOcclude transparent


{-|

    import Ui.Get key as Get key exposing (get)
    import Restrictive.Transformation exposing (OrAll(..))

    Get.full ()
        |> occludeOrAll (AllExcept [1, 2])
        |> get 2
        --> Just ()

    Get.full ()
        |> occludeOrAll (AllExcept [1, 2])
        |> get 3
        --> Nothing

-}
occludeOrAll : OrAll key -> Mask key a
occludeOrAll orAll =
    case orAll of
        All ->
            opaque

        Some list ->
            occludeList list

        AllExcept list ->
            List.foldl
                (\key mask_ ->
                    if List.member key list then
                        mask_

                    else
                        andOcclude key mask_
                )
                transparent
                list



---- COMPOSE ----


{-| Add occlusions

    import Ui.Get key exposing (Get)
    import Ui.Layout.Aspect exposing (Aspect(..))

    mask : Mask Aspect Bool
    mask = occlude Scene |> and (occlude Info)

    get : Get Aspect Bool
    get = mask (Ui.Get.full True)

    get Scene --> Nothing
    get Info --> Nothing
    get Control --> Just True

-}
and : Mask key a -> Mask key a -> Mask key a
and =
    (>>)


{-|

    Just
        |> invert transparent
        |> Get.get ()
        --> Nothing

    Just
        |> invert opaque
        |> Get.get ()
        --> Nothing

-}
invert : Mask key a -> Mask key a
invert mask_ getA key =
    case mask_ getA key of
        Just _ ->
            Nothing

        Nothing ->
            getA key


{-| Occlude one more aspect
-}
andOcclude : key -> Mask key a -> Mask key a
andOcclude =
    occlude
        >> and


{-| apply the mask only when the key meets a condition
-}
filter : (key -> Bool) -> Mask key a -> Mask key a
filter condition mask_ getA key =
    (if condition key then
        mask_ getA

     else
        getA
    )
        key


{-| -}
mapKey : ( key1 -> Maybe key2, key2 -> Maybe key1 ) -> Mask key2 a -> Mask key1 a
mapKey ( key12, key21 ) mask2 =
    -- mask is `Get key1 a -> Get key1 a`
    Get.mapKey key21
        >> mask2
        >> Get.mapKey key12


{-| combine a list of masks to a single masks. Nothing if list is empty.

    import Ui.Get
    import Ui.Layout.Aspect exposing (Aspect(..))

    Ui.Get.fromList [(Scene, "Scene"), (Control, "Control")]
        |> concat [occlude Scene, occlude Info]
        |> Ui.Get.values [Scene, Control, Info]
        --> ["Control"]

-}
concat : List (Mask key a) -> Mask key a
concat =
    List.foldl and transparent


{-| -}
mapSecond : (a -> b) -> Mask key a -> Get key a -> Get key b
mapSecond =
    mapParameter Get.map


{-| -}
map : (Mask aspect a -> b -> b) -> Mask aspect a -> Mask aspect b
map project mask_ =
    Get.map (project mask_)


{-| Apply the mask
-}
mask : Mask aspect a -> Get aspect a -> Get aspect a
mask =
    (<|)



--maskGet : Mask h -> v -> v


{-| This is a strangely important function that deserves an important name.
-}
mapParameter :
    (a
     -> (b -> c)
    )
    ->
        (a
         -> ((input -> b) -> (input -> c))
        )
mapParameter function =
    let
        composeRightToLeft : (b -> c) -> (input -> b) -> (input -> c)
        composeRightToLeft =
            (<<)
    in
    function >> composeRightToLeft
