module Ui.Layout.ViewModel exposing
    ( ViewModel, Foliage
    , empty
    , appendGet, mapGet, appendHandle, mapHandle
    , merge
    , concat, concatMap
    )

{-| Intermediate model calculated before applying a [Layout](Ui.Layout)

You can probably ignore this module.

@docs ViewModel, Foliage


# Create

@docs empty


# Map

@docs appendGet, mapGet, appendHandle, mapHandle


# Compose

@docs merge
@docs concat, concatMap

-}

import Html exposing (Html)
import Ui.Get as Get exposing (Get)


{-| -}
type alias ViewModel msg =
    { handle : Foliage msg
    , get : Get (Foliage msg)
    }


{-| -}
type alias Foliage msg =
    List ( String, Html msg )


{-| -}
empty : ViewModel msg
empty =
    { handle = [], get = Get.empty }


{-| Combine two ViewModels into one, concatenating its contents.

    import Ui.Aspect exposing (Aspect(..))

    merge
        { handle = [], get = Get.singleton Scene [] }
        { handle = [], get = Get.singleton Scene [] }
        |> {get} -> get Scene
    --> []

-}
merge : ViewModel msg -> ViewModel msg -> ViewModel msg
merge a b =
    { handle = a.handle ++ b.handle
    , get = Get.append a.get b.get
    }


{-| `concat = List.foldl merge empty`
-}
concat : List (ViewModel msg) -> ViewModel msg
concat =
    List.foldr merge empty


{-| Merge the results of `fu`
-}
concatMap : (a -> ViewModel msg) -> List a -> ViewModel msg
concatMap fu =
    List.map fu >> concat


{-| -}
mapGet : (Get (Foliage msg) -> Get (Foliage msg)) -> ViewModel msg -> ViewModel msg
mapGet fu =
    \v -> { v | get = fu v.get }


{-| -}
appendGet : Get (Foliage msg) -> ViewModel msg -> ViewModel msg
appendGet =
    Get.append >> mapGet


{-| -}
mapHandle : (Foliage msg -> Foliage msg) -> ViewModel msg -> ViewModel msg
mapHandle fu =
    \v -> { v | handle = fu v.handle }


{-| -}
appendHandle : Foliage msg -> ViewModel msg -> ViewModel msg
appendHandle foliage =
    mapHandle ((++) foliage)
