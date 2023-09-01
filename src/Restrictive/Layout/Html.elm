module Restrictive.Layout.Html exposing
    ( Ui
    , toggle, goTo, bounce
    , Wrapper(..), nest
    , layout
    , removed, removable, inserted, wrap, templates, arrange, concat
    )

{-| Default types and functions for working with [elm/html](https://package.elm-lang.org/packages/elm/html/latest/) within [`Restrictive.Ui`](Restrictive.Ui)

@docs Ui


# Create links

@docs toggle, goTo, bounce


# Wrap the DOM

@docs Wrapper, nest


# Layout

@docs layout


### Costruct your own:

@docs removed, removable, inserted, wrap, templates, arrange, concat

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Lazy
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout.Region as Region exposing (OrHeader(..), Region(..), withHeader)
import Restrictive.State as State
import Restrictive.Ui as Ui


{-| -}
type alias Ui msg narrowMsg =
    Ui.Ui
        Region
        (List (Html msg))
        (Wrapper narrowMsg msg)


{-| -}
type alias NarrowUi narrowMsg =
    Ui narrowMsg narrowMsg



---- Create links ----


{-| -}
toggle :
    List (Html.Attribute Never)
    ->
        { flag : State.Flag
        , isInline : Bool
        , label : List (Html msg)
        }
    -> Ui msg narrowMsg
    -> Ui msg narrowMsg
toggle attrs { flag, isInline, label } =
    State.toggle flag
        |> Link
            (templates attrs)
            { isInline = isInline
            , label = label
            }
        |> Ui.wrap


{-| -}
goTo :
    List (Html.Attribute Never)
    ->
        { destination : ( Maybe State.Path, State.Fragment )
        , isInline : Bool
        , label : List (Html msg)
        }
    -> Ui msg narrowMsg
    -> Ui msg narrowMsg
goTo attrs { destination, isInline, label } =
    State.goTo destination
        |> Link
            (templates attrs)
            { isInline = isInline
            , label = label
            }
        |> Ui.wrap


{-| -}
bounce :
    List (Html.Attribute Never)
    ->
        { here : ( Maybe State.Path, State.Fragment )
        , label : List (Html msg)
        , there : ( Maybe State.Path, State.Fragment )
        }
    -> Ui msg narrowMsg
    -> Ui msg narrowMsg
bounce attrs { here, label, there } =
    State.bounce { here = here, there = there }
        |> Link
            (templates attrs)
            { isInline = False
            , label = label
            }
        |> Ui.wrap



---- Working with contingent transformations ----


{-| Use with [`Ui.wrap`](Restrictive.Ui#wrap)

    Ui.wrap (Node "section" [ Attr.class "hello" ])

-}
type Wrapper narrowMsg msg
    = Node String (List (Html.Attribute msg))
    | Nest
        { combine :
            { makeInnerHtml :
                NarrowUi narrowMsg
                -> Maybe (List (Html narrowMsg))
            }
            -> List (Html msg)
        }
    | Link (State.Templates (List (Html msg))) (State.LinkStyle (List (Html msg))) State.Link


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
    -> Ui msg narrowMsg
nest config =
    Ui.wrap (Nest config) []



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
wrap : Wrapper narrowMsg msg -> Ui.Wrapper Region (List (Html narrowMsg)) (List (Html msg)) (Wrapper narrowMsg narrowMsg)
wrap =
    \wrapper ->
        case wrapper of
            Node str attrs ->
                Ui.WrapHtml (Html.node str attrs >> List.singleton)

            Nest { combine } ->
                Ui.Nest
                    { regions = [ Scene, Control, Info ]
                    , narrowLayout = layout
                    , combine = combine
                    }

            Link ele style link ->
                Ui.Link ele style link


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
        \{ url, label } ->
            [ Html.a (Attr.href url :: List.map (Attr.map never) attrs) label ]
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
    }
