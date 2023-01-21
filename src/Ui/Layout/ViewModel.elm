module Ui.Layout.ViewModel exposing
    ( ViewModel, Foliage
    , empty
    , appendGet, mapGet, appendHandle, mapHandle
    , merge
    , concat, concatMap
    , Transformation, applySimpleTransformation, vanish
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
import Html.Attributes as Attr
import Ui.Get as Get exposing (Get)
import Ui.Layout.Aspect exposing (Aspect)
import Ui.Mask exposing (Mask)


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


{-| Combine two ViewModels a and b into one, concatenating its contents like a++b.

    import Ui.Layout.Aspect exposing (Aspect(..))
    import Ui.Get

    merge
        { handle = [], get = Ui.Get.singleton Scene [] }
        { handle = [], get = Ui.Get.singleton Scene [] }
        |> \{ get } -> get Scene
    --> Just []

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


{-| Note that an `Aspect` of `Nothing` implies a global position.
-}
type alias Transformation msg =
    { occlude : List Aspect
    , append : ( Maybe Aspect, Foliage msg )
    }


{-| -}
vanish : ViewModel msg -> ViewModel msg
vanish =
    mapHandle poof
        >> mapGet (Get.map poof)


poof : Foliage msg -> Foliage msg
poof =
    List.intersperse ( "poof", Html.span [ Attr.class "poof" ] [ Html.text "" ] )
        >> (::) ( "poof", Html.span [ Attr.class "poof" ] [ Html.text "" ] )


{-| -}
applySimpleTransformation : Transformation msg -> ViewModel msg -> ViewModel msg
applySimpleTransformation { append } =
    let
        ( position, foliage ) =
            append
    in
    case position of
        Just aspect ->
            appendGet (Get.singleton aspect foliage)

        Nothing ->
            appendHandle foliage
