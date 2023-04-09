module Restrictive.Layout exposing
    ( Layout
    , sceneOnly, list
    , default, withClass
    )

{-| Lay out the [`ViewModel`](Ui.Layout.ViewModel)

For choosing as set of regions for your app, consult
[the corresponding w3 WAI tutorial](https://www.w3.org/WAI/tutorials/page-structure/regions/).

Note that `Restrictive` always assumes a `Header` region.

@docs Layout


# Defaults

@docs sceneOnly, list


## For Keyed Html

default, withClass

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Keyed exposing (node)
import Html.Lazy
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout.Region as Region exposing (OrHeader(..), Region(..), withHeader)


{-| The layout is a rule for mapping a Ui to an html tree.
-}
type alias Layout region wrapper html =
    { forget : wrapper
    , substitute : { current : wrapper, previous : wrapper }
    , regions : List region
    , wrap : wrapper -> List html -> List html
    , view : Get (OrHeader region) (List html) -> List html
    }



---- Defaults ----


{-| -}
sceneOnly : Layout Region (List html -> List html) html
sceneOnly =
    { forget = \_ -> []
    , substitute = { current = identity, previous = \_ -> [] }
    , view =
        Get.get (Region Scene)
            >> Maybe.withDefault []
    , wrap = identity
    , regions = Region.allAspects
    }


{-| -}
list : List region -> Layout region (List ( String, element ) -> List ( String, element )) ( String, element )
list allAspects =
    { forget = List.map <| \( _, v ) -> ( "-", v )
    , substitute = { current = identity, previous = \_ -> [] }
    , view = Get.concatValues (Header :: List.map Region allAspects)
    , wrap = identity
    , regions = allAspects
    }



---- Keyed Html ----


type alias KeyedHtmlWrapper msg =
    List ( String, Html msg ) -> List ( String, Html msg )


poof : List ( String, Html html ) -> List ( String, Html html )
poof =
    List.indexedMap (\i a -> [ ( "poof" ++ String.fromInt i, Html.span [ Attr.class "poof" ] [ Html.text (String.fromInt i) ] ), a ])
        >> List.concat


{-| -}
default : Layout Region (KeyedHtmlWrapper msg) ( String, Html msg )
default =
    { forget = poof
    , substitute = { current = identity, previous = \_ -> [] }
    , view =
        withHeader Region.allAspects
            |> Get.toListBy (niceLayout "")
    , wrap = identity
    , regions = Region.allAspects
    }


{-| -}
withClass : String -> Layout Region (KeyedHtmlWrapper msg) ( String, Html msg )
withClass prefix =
    { default
        | view =
            withHeader Region.allAspects
                |> Get.toListBy (niceLayout prefix)
    }


niceLayout : String -> Get (OrHeader Region) (List ( String, Html msg ) -> ( String, Html msg ))
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
