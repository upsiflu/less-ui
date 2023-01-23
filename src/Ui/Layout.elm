module Ui.Layout exposing
    ( Layout
    , default, sceneOnly, withClass, list
    )

{-| Lay out the [`ViewModel`](Ui.Layout.ViewModel)

@docs Layout

@docs default, sceneOnly, withClass, list

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Keyed exposing (node)
import Ui.Get as Get exposing (Get)
import Ui.Layout.Aspect as Aspect exposing (Aspect(..))
import Ui.Layout.ViewModel exposing (Foliage, ViewModel)


{-| -}
type alias Layout html =
    { view : ViewModel html -> Foliage html
    , markRemovals : Foliage html -> Foliage html
    }


{-| -}
default : Layout (Html msg)
default =
    { view =
        \{ handle, get } ->
            ( "handle", node "nav" [ Attr.class "handle" ] handle )
                :: Get.toListBy (niceLayout "") Aspect.all get
    , markRemovals = poof
    }


{-| -}
sceneOnly : Layout html
sceneOnly =
    { view =
        \{ get } ->
            get Scene
                |> Maybe.withDefault []
    , markRemovals = \_ -> []
    }


{-| -}
list : Layout html
list =
    { view =
        \{ handle, get } ->
            handle
                :: Get.values Aspect.all get
                |> List.concat
    , markRemovals = List.map (\( _, v ) -> ( "-", v ))
    }



---- Html ----


poof : Foliage (Html html) -> Foliage (Html html)
poof =
    List.indexedMap (\i a -> [ ( "poof" ++ String.fromInt i, Html.span [ Attr.class "poof" ] [ Html.text (String.fromInt i) ] ), a ])
        >> List.concat


{-| -}
withClass : String -> Layout (Html msg)
withClass prefix =
    { view =
        \{ handle, get } ->
            ( "handle", node "nav" [ Attr.class prefix, Attr.class "handle" ] handle )
                :: Get.toListBy (niceLayout prefix) Aspect.all get
    , markRemovals = poof
    }


niceLayout : String -> Get (Foliage (Html msg) -> ( String, Html msg ))
niceLayout prefix =
    Get.fromList
        [ ( Scene, node "div" [ Attr.class prefix, Attr.class "scene" ] >> Tuple.pair "scene" )
        , ( Control, node "div" [ Attr.class prefix, Attr.class "control" ] >> Tuple.pair "control" )
        , ( Info, node "div" [ Attr.class prefix, Attr.class "info" ] >> Tuple.pair "info" )
        ]
