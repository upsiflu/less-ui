module Ui.Layout.ViewModel exposing
    ( ViewModel, Foliage
    , empty
    , mapGet, mapHandle
    , merge
    )

{-| Intermediate model before applying a [Layout](Ui.Layout)

@docs ViewModel, Foliage


# Create

@docs empty


# Map

@docs mapGet, mapHandle


# Compose

@docs merge

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


{-| combines two ViewModels into one, concatenating its contents.

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
    , get = Get.concat a.get b.get
    }


{-| -}
mapGet : (Get (Foliage msg) -> Get (Foliage msg)) -> ViewModel msg -> ViewModel msg
mapGet fu =
    \v -> { v | get = fu v.get }


{-| -}
mapHandle : (Foliage msg -> Foliage msg) -> ViewModel msg -> ViewModel msg
mapHandle fu =
    \v -> { v | handle = fu v.handle }
