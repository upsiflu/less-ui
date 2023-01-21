module Ui.Layout.ViewModel exposing
    ( ViewModel, Foliage
    , empty
    , appendGet, mapGet, appendHandle, mapHandle
    , merge
    , concat, concatMap
    , Transformation, applyTransformations, maskFromTransformation, mergeDeprecated
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


{-| -}
mergeDeprecated : ViewModel msg -> ViewModel msg -> ViewModel msg
mergeDeprecated old new =
    mapHandle poof old
        |> mapGet (Get.map poof)
        |> merge new


{-| Note that an `Aspect` of `Nothing` implies a global position.
-}
type alias Transformation msg =
    { occlude : List Aspect
    , append : ( Maybe Aspect, Foliage msg )
    }


poof : Foliage msg -> Foliage msg
poof =
    (++) [ ( "poof", Html.span [ Attr.class "poof" ] [ Html.text "" ] ) ]


{-| As of now, we do detect:

  - Occlusion increases
  - Changes in the position (`Maybe Aspect`) of appended Foliage

We do not detect if Foliage changes in the very same place.

ERROR (WIP)

Ideas:

- Instead of injecting the `poof`, we want to
  declare, in Ui, a `wrapWithAnimation` or the like.

-}
applyTransformations :
    { maybeCurrent : Maybe (Transformation msg), maybePrevious : Maybe (Transformation msg) }
    -> ViewModel msg
    -> ViewModel msg
applyTransformations { maybeCurrent, maybePrevious } viewModel =
    let
        noTransformation =
            { occlude = [], append = ( Nothing, [] ) }

        ( current, previous ) =
            ( Maybe.withDefault noTransformation maybeCurrent, Maybe.withDefault noTransformation maybePrevious )

        --1. We want a ViewModel that only contains newly-occluded items.
        --1.a Apply the previous occlusion plus the inverse of the current occlusion
        deprecationMask =
            Ui.Mask.occludeList (previous.occlude ++ Ui.Layout.Aspect.inverse current.occlude)

        ( ( currentPosition, currentFoliage ), ( previousPosition, previousFoliage ) ) =
            ( current.append, previous.append )

        deprecationAppend =
            if currentPosition /= previousPosition then
                case Debug.log "ViewModel -> previous.append position" previousPosition of
                    Just aspect ->
                        appendGet (Get.singleton aspect (poof previousFoliage))

                    Nothing ->
                        appendHandle (poof previousFoliage)

            else
                identity

        currentAppend =
            case Debug.log "ViewModel -> current.append position" currentPosition of
                Nothing ->
                    appendHandle currentFoliage

                Just aspect ->
                    appendGet (Get.singleton aspect currentFoliage)

        deprecationModel =
            viewModel
                |> mapGet (deprecationMask >> Get.map poof)
                |> deprecationAppend

        currentModel =
            viewModel
                |> mapGet (Ui.Mask.occludeList current.occlude)
                |> currentAppend
    in
    merge currentModel deprecationModel


{-| -}
maskFromTransformation : { a | occlude : List Aspect } -> Mask b
maskFromTransformation { occlude } =
    Ui.Mask.occludeList occlude
