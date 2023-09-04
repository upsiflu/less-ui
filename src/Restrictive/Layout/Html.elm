module Restrictive.Layout.Html exposing
    ( Ui
    , toggle, goTo, bounce
    , Wrapper(..), ol, ul, keyedNode, nest
    , layout
    , removed, removable, inserted, wrap, templates, arrange, concat
    , assign
    )

{-| Default types and functions for working with [elm/html](https://package.elm-lang.org/packages/elm/html/latest/) within [`Restrictive.Ui`](Restrictive.Ui)

@docs Ui


# Create Links

_For use cases, read [the Create Links section in the State module](Restrictive-State#create-links)_

@docs toggle, goTo, bounce


# Wrap the DOM

@docs Wrapper, ol, ul, keyedNode, nest


# Layout

@docs layout


### Costruct your own:

@docs removed, removable, inserted, wrap, templates, arrange, concat

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Keyed
import Html.Lazy
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout.Region as Region exposing (OrHeader(..), Region(..), withHeader)
import Restrictive.State as State
import Restrictive.Ui as Ui


{-| -}
type alias Ui narrowMsg msg =
    Ui.Ui
        Region
        (List (Html msg))
        (Wrapper narrowMsg msg)


{-| -}
type alias NarrowUi narrowMsg =
    Ui narrowMsg narrowMsg



---- Create links ----


{-| Toggle a `flag` and show/hide the nested `Ui` accordingly.
-}
toggle :
    List (Html.Attribute Never)
    ->
        { flag : State.Flag
        , isInline : Bool
        , label : List (Html msg)
        }
    -> Ui narrowMsg msg
    -> Ui narrowMsg msg
toggle attrs { flag, isInline, label } =
    Link
        (templates attrs)
        { isInline = isInline
        , label = label
        }
        (State.toggle flag)
        >> Ui.wrap


{-| Toggle a `flag` and show/hide the nested `Ui` accordingly.
-}
assign :
    List (Html.Attribute Never)
    ->
        { category : State.Flag
        , isInline : Bool
        , label : List (Html msg)
        }
    -> String
    -> Ui narrowMsg msg
    -> Ui narrowMsg msg
assign attrs { category, isInline, label } searchTerm =
    Link
        (templates attrs)
        { isInline = isInline
        , label = label
        }
        (State.assign ( category, searchTerm ))
        >> Ui.wrap


{-| Navigate to a `destination` and show the nested `Ui` when it's reached.
-}
goTo :
    List (Html.Attribute Never)
    ->
        { destination : ( Maybe State.Path, State.Fragment )
        , isInline : Bool
        , label : List (Html msg)
        }
    -> Ui narrowMsg msg
    -> Ui narrowMsg msg
goTo attrs { destination, isInline, label } =
    Link
        (templates attrs)
        { isInline = isInline
        , label = label
        }
        (State.goTo destination)
        >> Ui.wrap


{-| Navigate to `there`, and from there, back `here`.
-}
bounce :
    List (Html.Attribute Never)
    ->
        { here : ( Maybe State.Path, State.Fragment )
        , label : List (Html msg)
        , there : ( Maybe State.Path, State.Fragment )
        }
    -> Ui narrowMsg msg
    -> Ui narrowMsg msg
bounce attrs { here, label, there } =
    Link
        (templates attrs)
        { isInline = False
        , label = label
        }
        (State.bounce { here = here, there = there })
        >> Ui.wrap



---- Working with contingent transformations ----


{-| Use with [`Ui.wrap`](Restrictive.Ui#wrap)

    Ui.wrap (Node "section" [ Attr.class "hello" ])

-}
type Wrapper narrowMsg msg
    = Node String (List (Html.Attribute msg)) (Ui narrowMsg msg)
    | Nest
        { combine :
            { makeInnerHtml :
                NarrowUi narrowMsg
                -> Maybe (List (Html narrowMsg))
            }
            -> List (Html msg)
        }
    | Link (State.Templates (List (Html msg))) (State.LinkStyle (List (Html msg))) State.Link (Ui narrowMsg msg)
    | Keyed (List ( String, List (Html msg) ) -> List (Html msg)) (List ( String, Ui narrowMsg msg ))


applyKeyedFu : (List ( String, Html msg ) -> Html msg) -> (List ( String, List (Html msg) ) -> List (Html msg))
applyKeyedFu fu =
    List.concatMap
        (\( key, items ) ->
            List.indexedMap (\i a -> ( key ++ "." ++ String.fromInt i, a )) items
        )
        >> fu
        >> List.singleton


{-| -}
ol : List (Html.Attribute msg) -> List ( String, Ui narrowMsg msg ) -> Ui narrowMsg msg
ol attrs =
    Keyed (applyKeyedFu (Html.Keyed.ol attrs)) >> Ui.wrap


{-| -}
ul : List (Html.Attribute msg) -> List ( String, Ui narrowMsg msg ) -> Ui narrowMsg msg
ul attrs =
    Keyed (applyKeyedFu (Html.Keyed.ul attrs)) >> Ui.wrap


{-| -}
keyedNode : String -> List (Html.Attribute msg) -> List ( String, Ui narrowMsg msg ) -> Ui narrowMsg msg
keyedNode tagName attrs =
    Keyed (applyKeyedFu (Html.Keyed.node tagName attrs)) >> Ui.wrap


{-| Gives your Html widgets access to state information.

For example, if you want to extend a widget or form generator (`elm-any-type-forms`) that can only output Html
with `Ui` elements that alter and respond to the Url, then you need

  - a way to convert from `Ui` to `html` -> `view`
  - a way to convert from `html` to `Ui` -> `singleton`
  - a way to forward the current state to the nested `Ui`

Here is how you use this function:

1.  Write the `Ui` code for your widget extension.
    You can use all the local parameters your widget provides.

2.  Convert it to `html` using the `makeInnerHtml` function. Just pretend it exists:

    { makeInnerHtml } ->
    Ui.singleton...
    |> makeInnerHtml

    Note that the inner html will not bubble any messages to your app, so you are limited to Url-based state.

3.  Your widget will create `Html msg`. This will be the parameter you provide `stateful`.
    It will need a `Layout` to render `Ui (Html Never)` into `Html Never`.

Caution: If you use this functionality, the `Ui` will contain functions and will no longer support equality checks and serialisation.

-}
nest :
    { combine :
        { makeInnerHtml :
            NarrowUi narrowMsg
            -> Maybe (List (Html narrowMsg))
        }
        -> List (Html msg)
    }
    -> Ui narrowMsg msg
nest config =
    Ui.wrap (Nest config)



----- LAYOUT -----


{-| -}
layout :
    Ui.Layout
        Region
        (List (Html narrowMsg))
        (List (Html msg))
        (Wrapper narrowMsg narrowMsg)
        (Wrapper narrowMsg msg)
layout =
    { removed = removed
    , removable = removable
    , inserted = inserted
    , wrap = wrap
    , concat = concat
    , arrange = arrange
    }


{-| -}
removed : List (Html msg) -> List (Html msg)
removed =
    List.map (\a -> Html.span [ Attr.class "removed", Attr.attribute "aria-hidden" "true", Attr.tabindex -1 ] [ a ])


{-| -}
removable : List (Html msg) -> List (Html msg)
removable =
    List.map (\a -> Html.span [ Attr.class "removable" ] [ a ])


{-| -}
inserted : List (Html msg) -> List (Html msg)
inserted =
    List.map (\a -> Html.span [ Attr.class "inserted removable" ] [ a ])


{-| -}
wrap : Wrapper narrowMsg msg -> Ui.Wrapper Region (List (Html narrowMsg)) (List (Html msg)) (Wrapper narrowMsg narrowMsg) (Wrapper narrowMsg msg)
wrap =
    \wrapper ->
        case wrapper of
            Node str attrs elements ->
                Ui.WrapHtml (Html.node str attrs >> List.singleton) elements

            Nest { combine } ->
                Ui.Nest
                    { regions = [ Scene, Control, Info ]
                    , narrowLayout = layout
                    , combine = combine
                    }

            Link template style link elements ->
                Ui.Link template style link elements

            Keyed fu list ->
                Ui.Keyed fu list


{-| -}
concat : List (List a) -> List a
concat =
    List.concat


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


{-| -}
templates : List (Html.Attribute Never) -> State.Templates (List (Html msg_))
templates attrs =
    { link =
        \{ url, label, isCurrent } ->
            [ Html.a
                (Attr.href url
                    :: Attr.attribute "aria-current"
                        (if isCurrent then
                            "page"

                         else
                            "false"
                        )
                    :: List.map (Attr.map never) attrs
                )
                label
            ]
    , switch =
        \{ url, label, isChecked } ->
            [ Html.a
                (Attr.href url
                    :: Attr.attribute "role" "switch"
                    :: Attr.attribute "aria-checked"
                        (if isChecked then
                            "true"

                         else
                            "false"
                        )
                    :: List.map (Attr.map never) attrs
                )
                label
            ]
    , search =
        \{ assignment, label, isCurrent } ->
            let
                ( category, searchTerm ) =
                    assignment
            in
            Html.input
                (Attr.value searchTerm
                    :: Attr.title category
                    :: Attr.attribute "aria-current"
                        (if isCurrent then
                            "page"

                         else
                            "false"
                        )
                    :: List.map (Attr.map never) attrs
                )
                []
                :: label
    }
