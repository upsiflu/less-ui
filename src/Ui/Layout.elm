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
import Ui.Layout.ViewModel exposing (ViewModel)


{-| -}
type alias Layout place html wrapper =
    { forget : wrapper
    , wrap : wrapper -> List html -> List html
    , view : ViewModel (Maybe place) html -> List html
    , all : List place
    }


{-| -}
default : Layout Aspect ( String, Html msg ) (List ( String, Html msg ) -> List ( String, Html msg ))
default =
    { forget = poof
    , wrap = identity
    , view =
        \{ get } ->
            Get.toListBy (niceLayout "") (Nothing :: List.map Just Aspect.all) get
    , all = Aspect.all
    }


{-| -}
sceneOnly : Layout Aspect html (List html -> List html)
sceneOnly =
    { forget = \_ -> []
    , wrap = identity
    , view =
        \{ get } ->
            get (Just Scene)
                |> Maybe.withDefault []
    , all = Aspect.all
    }



-- aspect = aspect
-- html = (String, element)
-- wrapper = (List ( String, element ) -> List ( String, element ))
-- markRemovals : Mask aspect (List (String, element))
-- wrap :
-- view : ViewModel aspect (String, element) -> List (String, element)


{-| -}
list : List aspect -> Layout aspect ( String, element ) (List ( String, element ) -> List ( String, element ))
list allAspects =
    { forget = List.map <| \( _, v ) -> ( "-", v )
    , wrap = identity
    , view =
        \{ get } ->
            Get.values (Nothing :: List.map Just allAspects) get
                |> List.concat
    , all = allAspects
    }



---- Html ----


poof : List ( String, Html html ) -> List ( String, Html html )
poof =
    List.indexedMap (\i a -> [ ( "poof" ++ String.fromInt i, Html.span [ Attr.class "poof" ] [ Html.text (String.fromInt i) ] ), a ])
        >> List.concat


{-| -}
withClass : String -> Layout Aspect ( String, Html msg ) (List ( String, Html msg ) -> List ( String, Html msg ))
withClass prefix =
    { forget = poof
    , wrap = identity
    , view =
        \{ get } -> Get.toListBy (niceLayout prefix) (Nothing :: List.map Just Aspect.all) get
    , all = Aspect.all
    }


niceLayout : String -> Get (Maybe Aspect) (List ( String, Html msg ) -> ( String, Html msg ))
niceLayout prefix =
    Get.fromList
        [ ( Nothing, node "nav" [ Attr.class prefix, Attr.class "handle" ] >> Tuple.pair "handle" )
        , ( Just Scene, node "div" [ Attr.class prefix, Attr.class "scene" ] >> Tuple.pair "scene" )
        , ( Just Control, node "div" [ Attr.class prefix, Attr.class "control" ] >> Tuple.pair "control" )
        , ( Just Info, node "div" [ Attr.class prefix, Attr.class "info" ] >> Tuple.pair "info" )
        ]
