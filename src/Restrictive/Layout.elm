module Restrictive.Layout exposing
    ( Layout
    , default, sceneOnly, withClass, list
    )

{-| Lay out the [`ViewModel`](Ui.Layout.ViewModel)

For choosing as set of regions for your app, consult
[the corresponding w3 WAI tutorial](https://www.w3.org/WAI/tutorials/page-structure/regions/).

Note that `Restrictive` always assumes a `Header` region.

@docs Layout

@docs default, sceneOnly, withClass, list

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Keyed exposing (node)
import Html.Lazy
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout.Region as Region exposing (Aspect(..), OrHeader(..), withHeader)


{-| -}
type alias Layout region html wrapper =
    { forget : wrapper
    , substitute : { current : wrapper, previous : wrapper }
    , wrap : wrapper -> List html -> List html
    , view : Get (OrHeader region) (List html) -> List html
    , regions : List region
    }


{-| -}
default : Layout Aspect ( String, Html msg ) (List ( String, Html msg ) -> List ( String, Html msg ))
default =
    { forget = poof
    , substitute = { current = identity, previous = \_ -> [] }
    , wrap = identity
    , view =
        withHeader Region.allAspects
            |> Get.toListBy (niceLayout "")
    , regions = Region.allAspects
    }


{-| -}
sceneOnly : Layout Aspect html (List html -> List html)
sceneOnly =
    { forget = \_ -> []
    , substitute = { current = identity, previous = \_ -> [] }
    , wrap = identity
    , view =
        Get.get (Region Scene)
            >> Maybe.withDefault []
    , regions = Region.allAspects
    }


{-| -}
list : List region -> Layout region ( String, element ) (List ( String, element ) -> List ( String, element ))
list allAspects =
    { forget = List.map <| \( _, v ) -> ( "-", v )
    , substitute = { current = identity, previous = \_ -> [] }
    , wrap = identity
    , view = Get.concatValues (Header :: List.map Region allAspects)
    , regions = allAspects
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
    , substitute = { current = identity, previous = \_ -> [] }
    , view =
        withHeader Region.allAspects
            |> Get.toListBy (niceLayout prefix)
    , regions = Region.allAspects
    }


niceLayout : String -> Get (OrHeader Aspect) (List ( String, Html msg ) -> ( String, Html msg ))
niceLayout prefix =
    Get.fromList
        [ ( Header
          , Html.Lazy.lazy3 node "nav" [ Attr.class prefix, Attr.class "handle" ] >> Tuple.pair "handle"
          )
        , ( Region Scene
          , Html.Lazy.lazy3 node "main" [ Attr.class prefix, Attr.class "scene" ] >> Tuple.pair "scene"
          )
        , ( Region Control
          , Html.Lazy.lazy3 node "div" [ Attr.class prefix, Attr.class "control" ] >> Tuple.pair "control"
          )
        , ( Region Info
          , Html.Lazy.lazy3 node "div" [ Attr.class prefix, Attr.class "info" ] >> Tuple.pair "info"
          )
        ]
