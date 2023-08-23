module Restrictive.Layout.Html.Keyed exposing (Ui, default)

{-| Layout functions specific to the Ui library

    Html.Keyed

Use the `default` Layout or override any of its fields.

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Keyed exposing (node)
import Html.Lazy
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout exposing (Layout)
import Restrictive.Layout.Region as Region exposing (OrHeader(..), Region(..), withHeader)
import Restrictive.State
import Restrictive.Ui


{-| -}
default : Layout Region (List ( String, Html msg )) (Html.Attribute Never) (KeyedHtmlWrapper msg)
default =
    { remove = Poof
    , insert = Identity
    , wrap =
        \wrapper children ->
            case wrapper of
                Node str attrs ->
                    [ ( str, node str attrs children ) ]

                Poof ->
                    poof children

                Identity ->
                    children
    , elements = elements
    , concat = List.concat
    , arrange =
        withHeader Region.allRegions
            --Todo: Add custom class variant
            |> Get.toListBy (niceLayout "")
    }


{-| -}
type alias Ui msg =
    Restrictive.Ui.Ui
        Region
        (List ( String, Html msg ))
        (Html.Attribute Never)
        (KeyedHtmlWrapper msg)


{-| -}
type KeyedHtmlWrapper msg
    = Node String (List (Html.Attribute msg))
    | Poof
    | Identity


poof : List ( String, Html msg ) -> List ( String, Html msg )
poof =
    List.indexedMap (\i a -> [ ( "poof" ++ String.fromInt i, Html.span [ Attr.class "poof" ] [ Html.text (String.fromInt i) ] ), a ])
        >> List.concat


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


elements : Restrictive.State.Elements (List ( String, Html msg )) (Html.Attribute Never)
elements =
    { link =
        \attr { url, label } ->
            [ ( url, node "a" (Attr.href url :: List.map (Attr.map never) attr) (List.concat label) ) ]
    , switch =
        \attr { url, label, isChecked } ->
            [ ( url
              , node "a"
                    (Attr.href url
                        :: Attr.attribute "role" "switch"
                        :: Attr.attribute "aria-checked"
                            (if isChecked then
                                "true"

                             else
                                "false"
                            )
                        :: List.map (Attr.map never) attr
                    )
                    (List.concat label)
              )
            ]
    }
