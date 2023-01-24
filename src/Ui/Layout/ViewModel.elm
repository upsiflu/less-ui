module Ui.Layout.ViewModel exposing
    ( ViewModel
    , empty, full
    , appendGet, maskGet, append
    , merge
    , concat, concatMap
    )

{-| Intermediate model calculated before applying a [Layout](Ui.Layout)

You can probably ignore this module.

@docs ViewModel


# Create

@docs empty, full


# Map

@docs appendGet, maskGet, append


# Compose

@docs merge
@docs concat, concatMap

-}

import Ui.Get as Get exposing (Get)


{-| -}
type alias ViewModel aspect html =
    { get : Get aspect (List html) }


{-| -}
empty : ViewModel aspect html
empty =
    { get = Get.empty }


{-| -}
full : List html -> ViewModel aspect html
full html =
    { get = Get.full html }


{-| Combine two ViewModels a and b into one, concatenating its contents like a++b.

    import Ui.Layout.Aspect exposing (Aspect(..))
    import Ui.Get

    merge
        { handle = [], get = Ui.Get.singleton Scene [] }
        { handle = [], get = Ui.Get.singleton Scene [] }
        |> \{ get } -> get Scene
    --> Just []

-}
merge : ViewModel aspect html -> ViewModel aspect html -> ViewModel aspect html
merge a b =
    { get = Get.append a.get b.get }


{-| `concat = List.foldl merge empty`
-}
concat : List (ViewModel aspect html) -> ViewModel aspect html
concat =
    List.foldr merge empty


{-| Merge the results of `fu`
-}
concatMap : (a -> ViewModel aspect html) -> List a -> ViewModel aspect html
concatMap fu =
    List.map fu >> concat


{-| -}
maskGet : (Get aspect (List html) -> Get aspect (List html)) -> ViewModel aspect html -> ViewModel aspect html
maskGet mask =
    \v -> { v | get = mask v.get }


{-| -}
appendGet : Get aspect (List html) -> ViewModel aspect html -> ViewModel aspect html
appendGet =
    Get.append >> maskGet


{-| Note that `Nothing` means the handle is appended. (ToDo: Make a nicer sum type to represent position)
-}
append : { a | appendWhat : List html, appendWhere : Maybe aspect } -> ViewModel (Maybe aspect) html -> ViewModel (Maybe aspect) html
append { appendWhat, appendWhere } =
    Get.singleton appendWhere appendWhat
        |> appendGet
