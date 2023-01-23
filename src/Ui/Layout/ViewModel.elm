module Ui.Layout.ViewModel exposing
    ( ViewModel, Foliage, Keyed
    , empty
    , appendGet, mapGet, appendHandle, mapHandle, append
    , merge
    , concat, concatMap
    )

{-| Intermediate model calculated before applying a [Layout](Ui.Layout)

You can probably ignore this module.

@docs ViewModel, Foliage, Keyed


# Create

@docs empty


# Map

@docs appendGet, mapGet, appendHandle, mapHandle, append


# Compose

@docs merge
@docs concat, concatMap

-}

import Ui.Get as Get exposing (Get)
import Ui.Layout.Aspect exposing (Aspect)


{-| -}
type alias ViewModel html =
    { handle : Foliage html
    , get : Get (Foliage html)
    }


{-| -}
type alias Foliage html =
    List ( String, html )


{-| -}
type alias Keyed html =
    ( String, html )


{-| -}
empty : ViewModel html
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
merge : ViewModel html -> ViewModel html -> ViewModel html
merge a b =
    { handle = a.handle ++ b.handle
    , get = Get.append a.get b.get
    }


{-| `concat = List.foldl merge empty`
-}
concat : List (ViewModel html) -> ViewModel html
concat =
    List.foldr merge empty


{-| Merge the results of `fu`
-}
concatMap : (a -> ViewModel html) -> List a -> ViewModel html
concatMap fu =
    List.map fu >> concat


{-| -}
mapGet : (Get (Foliage html) -> Get (Foliage html)) -> ViewModel html -> ViewModel html
mapGet fu =
    \v -> { v | get = fu v.get }


{-| -}
appendGet : Get (Foliage html) -> ViewModel html -> ViewModel html
appendGet =
    Get.append >> mapGet


{-| -}
mapHandle : (Foliage html -> Foliage html) -> ViewModel html -> ViewModel html
mapHandle fu =
    \v -> { v | handle = fu v.handle }


{-| Note that appending is to the left.
-}
appendHandle : Foliage html -> ViewModel html -> ViewModel html
appendHandle foliage =
    mapHandle (List.append foliage)


{-| Note that `Nothing` means the handle is appended. (ToDo: Make a nicer sum type to represent position)
-}
append : { a | appendWhat : Foliage html, appendWhere : Maybe Aspect } -> ViewModel html -> ViewModel html
append { appendWhat, appendWhere } =
    case appendWhere of
        Just aspect ->
            Get.singleton aspect appendWhat
                |> appendGet

        Nothing ->
            appendHandle appendWhat
