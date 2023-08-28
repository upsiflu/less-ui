module Restrictive.Layout.Html exposing
    ( layout, Wrapper(..)
    , wrap, elements, arrange
    , staticLayout
    , toHtml
    )

{-| Layout functions specific to the Ui library

    Html.Keyed


# Use the defaults...

@docs layout, Wrapper


# ...or override any of its fields:

@docs wrap, elements, arrange


# Special case: a layout that cannot emit any messages

@docs staticLayout

---


# View

@docs toHtml

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Lazy
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout exposing (Layout)
import Restrictive.Layout.Region as Region exposing (OrHeader(..), Region(..), withHeader)
import Restrictive.State


{-| -}
toHtml : List (Html msg) -> List (Html msg)
toHtml =
    identity


{-| -}
layout : Layout Region (List (Html msg)) (Html.Attribute Never) (Wrapper msg)
layout =
    { removed = Removed
    , removable = Removable
    , inserted = Inserted
    , wrap = wrap
    , elements = elements
    , concat = List.concat
    , arrange = arrange
    }


{-| -}
staticLayout : Layout Region (List (Html Never)) (Html.Attribute Never) (Wrapper Never)
staticLayout =
    { removed = Removed
    , removable = Removable
    , inserted = Inserted
    , wrap = wrap
    , elements = elements
    , concat = List.concat
    , arrange = arrange
    }


{-| Todo:
collapsible (proxyLabel):

    summary
        (details proxyLabel)
        :: children

dismissible id:

    summary
        (details "")
        :: "x"
        :: children

In each case, the `details` should be rendered without triangle and instead in the special color.
We should implement it not with `details` but with a special css element where ...

-}
type Wrapper msg
    = Node String (List (Html.Attribute msg))
    | Removed
    | Removable
    | Inserted


wrap : Wrapper msg -> List (Html msg) -> List (Html msg)
wrap wrapper children =
    case wrapper of
        Node str attrs ->
            [ Html.node str attrs children ]

        Removed ->
            List.map
                (\a -> Html.span [ Attr.class "removed", Attr.attribute "aria-hidden" "true", Attr.tabindex -1 ] [ a ])
                children

        Removable ->
            List.map
                (\a -> Html.span [ Attr.class "removable" ] [ a ])
                children

        Inserted ->
            List.map
                (\a -> Html.span [ Attr.class "inserted removable" ] [ a ])
                children


elements : Restrictive.State.Elements (List (Html msg_)) (Html.Attribute Never)
elements =
    { link =
        \attr { url, label } ->
            [ Html.a (Attr.href url :: List.map (Attr.map never) attr) label ]
    , switch =
        \attr { url, label, isChecked } ->
            [ Html.a
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
            ]
    }


{-| -}
arrange : Get (OrHeader Region) (List (Html msg)) -> List (Html msg)
arrange =
    withHeader Region.allRegions
        |> Get.toListBy
            (Get.fromList
                [ ( Header
                  , Html.Lazy.lazy2 Html.header [ Attr.class "header" ]
                  )
                , ( Region Scene
                  , Html.Lazy.lazy2 Html.main_ [ Attr.class "scene" ]
                  )
                , ( Region Control
                  , Html.Lazy.lazy2 Html.div [ Attr.class "control" ]
                  )
                , ( Region Info
                  , Html.Lazy.lazy2 Html.div [ Attr.class "info" ]
                  )
                ]
            )
