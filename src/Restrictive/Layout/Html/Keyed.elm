module Restrictive.Layout.Html.Keyed exposing (default, Ui, Document, Wrapper(..))

{-| Layout functions specific to the Ui library

    Html.Keyed

Use the `default` Layout or override any of its fields.

@docs default, Ui, Document, Wrapper

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Keyed exposing (node)
import Html.Lazy
import Restrictive
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout exposing (Layout)
import Restrictive.Layout.Region as Region exposing (OrHeader(..), Region(..), withHeader)
import Restrictive.State
import Restrictive.Ui


{-| -}
default : Layout Region (List ( String, Html msg )) (Html.Attribute Never) (Wrapper msg)
default =
    { remove = Poof
    , insert = Identity
    , wrap =
        \wrapper children ->
            case wrapper of
                Node str attrs ->
                    [ ( str, node str attrs children ) ]

                Ul attrs ->
                    [ ( "ul", Html.Keyed.ul attrs children ) ]

                Ol attrs ->
                    [ ( "ul", Html.Keyed.ol attrs children ) ]

                Poof ->
                    poof2 children

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
        (Wrapper msg)


{-| -}
type alias Document msg =
    Restrictive.Document
        Region
        (List ( String, Html msg ))
        (Html.Attribute Never)
        (Wrapper msg)


{-| -}
type Wrapper msg
    = Node String (List (Html.Attribute msg))
    | Ol (List (Html.Attribute msg))
    | Ul (List (Html.Attribute msg))
    | Poof
    | Identity


poof : List ( String, Html msg ) -> List ( String, Html msg )
poof =
    List.indexedMap (\i ( s, _ ) -> [ ( s, Html.span [ Attr.class "poof" ] [ Html.text (String.fromInt i) ] ) ])
        >> List.concat


poof2 : List ( String, Html msg ) -> List ( String, Html msg )
poof2 =
    List.indexedMap (\i ( s, _ ) -> [ ( s, Html.span [ Attr.class "poof" ] [ Html.img [ Attr.src "https://i.gifer.com/3klP.gif" ] [] ] ) ])
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
            [ ( url, node "a" (Attr.href url :: List.map (Attr.map never) attr) label ) ]
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
                    label
              )
            ]
    }
