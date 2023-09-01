module Restrictive.Layout.Html.Keyed exposing
    ( Ui
    , toggle, goTo, bounce
    , Wrapper(..), ol, ul, nest
    , layout, toHtml
    , removed, removable, inserted, wrap, arrange, concat, templates
    )

{-| Default types and functions for working with [`Html.Keyed`](https://package.elm-lang.org/packages/elm/html/latest/Html.Keyed) within [`Restrictive.Ui`](Restrictive.Ui)

Annotates nodes for Elm's runtime vDom differ so it can reuse nodes even when they change position. If you encounter problems with inputs or custom elements resetting when their position within the parent element changes, this annotation may help.

@docs Ui


# Create links

@docs toggle, goTo, bounce


# Wrap the DOM

@docs Wrapper, ol, ul, nest


# Layout

@docs layout, toHtml


### Costruct your own:

@docs removed, removable, inserted, wrap, arrange, concat, templates

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Keyed as Keyed
import Html.Lazy
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout.Region as Region exposing (OrHeader(..), Region(..), withHeader)
import Restrictive.State as State
import Restrictive.Ui as Ui


{-| -}
type alias Ui msg narrowMsg =
    Ui.Ui
        Region
        (List ( String, Html msg ))
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
        , label : List ( String, Html msg )
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
        , label : List ( String, Html msg )
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
        , label : List ( String, Html msg )
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


{-|

    Ui.wrap (Node "section" [ Attr.class "hello" ])

-}
type Wrapper narrowMsg msg
    = Node String (List (Html.Attribute msg))
    | Nest
        { combine :
            { makeInnerHtml :
                NarrowUi narrowMsg
                -> Maybe (List ( String, Html narrowMsg ))
            }
            -> List ( String, Html msg )
        }
    | Link (State.Templates (List ( String, Html msg ))) (State.LinkStyle (List ( String, Html msg ))) State.Link


{-| -}
ol : List (Html.Attribute msg) -> Ui msg narrowMsg -> Ui msg narrowMsg
ol =
    Node "ol" >> Ui.wrap


{-| -}
ul : List (Html.Attribute msg) -> Ui msg narrowMsg -> Ui msg narrowMsg
ul =
    Node "ul" >> Ui.wrap


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
            -> Maybe (List ( String, Html narrowMsg ))
        }
        -> List ( String, Html msg )
    }
    -> Ui msg narrowMsg
nest config =
    Ui.wrap (Nest config) []



----- LAYOUT -----


{-| -}
layout :
    Ui.Layout
        Region
        (List ( String, Html narrowMsg ))
        (List ( String, Html msg ))
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
removed : List ( String, Html msg ) -> List ( String, Html msg )
removed =
    List.map (\( str, a ) -> ( str, Html.span [ Attr.class "removed", Attr.attribute "aria-hidden" "true", Attr.tabindex -1 ] [ a ] ))


{-| -}
removable : List ( String, Html msg ) -> List ( String, Html msg )
removable =
    List.map (\( str, a ) -> ( str, Html.span [ Attr.class "removable" ] [ a ] ))


{-| -}
inserted : List ( String, Html msg ) -> List ( String, Html msg )
inserted =
    List.map (\( str, a ) -> ( str, Html.span [ Attr.class "inserted removable" ] [ a ] ))


{-| -}
wrap : Wrapper narrowMsg msg -> Ui.Wrapper Region (List ( String, Html narrowMsg )) (List ( String, Html msg )) (Wrapper narrowMsg narrowMsg)
wrap =
    \wrapper ->
        case wrapper of
            Node str attrs ->
                Ui.WrapHtml (Keyed.node str attrs >> Tuple.pair str >> List.singleton)

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
arrange : Get (OrHeader Region) (List ( String, Html msg )) -> List ( String, Html msg )
arrange =
    withHeader Region.allRegions
        |> Get.toListBy
            (Get.fromList
                [ ( Header
                  , Html.Lazy.lazy3 Keyed.node "header" [ Attr.class "header" ] >> Tuple.pair "header"
                  )
                , ( Region Scene
                  , Html.Lazy.lazy3 Keyed.node "main" [ Attr.class "scene" ] >> Tuple.pair "scene"
                  )
                , ( Region Control
                  , Html.Lazy.lazy3 Keyed.node "div" [ Attr.class "control" ] >> Tuple.pair "control"
                  )
                , ( Region Info
                  , Html.Lazy.lazy3 Keyed.node "div" [ Attr.class "info" ] >> Tuple.pair "info"
                  )
                ]
            )


{-| -}
templates : List (Html.Attribute Never) -> State.Templates (List ( String, Html msg_ ))
templates attrs =
    { link =
        \{ url, label } ->
            [ ( "", Keyed.node "a" (Attr.href url :: List.map (Attr.map never) attrs) label ) ]
    , switch =
        \{ url, label, isChecked } ->
            [ ( ""
              , Keyed.node "a"
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
              )
            ]
    }


{-| -}
toHtml : List ( String, Html msg ) -> List (Html msg)
toHtml =
    List.map Tuple.second
