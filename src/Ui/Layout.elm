module Ui.Layout exposing
    ( Layout(..)
    , view
    )

{-| Lay out the [`ViewModel`](Ui.Layout.ViewModel)

@docs Layout


# View

@docs view

-}

import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Keyed exposing (node)
import Ui.Get as Get exposing (Get)
import Ui.Layout.Aspect exposing (Aspect(..))
import Ui.Layout.ViewModel exposing (Foliage, ViewModel)


{-| -}
type Layout
    = Default
    | SceneOnly
    | WithClass String


{-| Traverses the tree from leaves to root, building up the vDom
-}
view : ViewModel msg -> Layout -> Foliage msg
view { handle, get } layout =
    case layout of
        Default ->
            ( "handle", node "nav" [ class "handle" ] handle )
                :: Get.toListBy (niceLayout "") [ Scene, Info, Control ] get

        SceneOnly ->
            get Scene
                |> Maybe.withDefault []

        WithClass prefix ->
            ( "handle", node "nav" [ class prefix, class "handle" ] handle )
                :: Get.toListBy (niceLayout prefix) [ Scene, Info, Control ] get


niceLayout : String -> Get (Foliage msg -> ( String, Html msg ))
niceLayout prefix =
    Get.fromList
        [ ( Scene, node "div" [ class prefix, class "scene" ] >> Tuple.pair "scene" )
        , ( Control, node "div" [ class prefix, class "control" ] >> Tuple.pair "control" )
        , ( Info, node "div" [ class prefix, class "info" ] >> Tuple.pair "info" )
        ]
