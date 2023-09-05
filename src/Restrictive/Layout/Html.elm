module Restrictive.Layout.Html exposing
    ( Ui, singleton
    , toggle, goTo, bounce, filter
    , Wrapper(..), ol, ul, keyedNode, nest
    , layout
    , removed, removable, inserted, wrap, getTemplates, arrange, concat
    )

{-| Default types and functions for working with [elm/html](https://package.elm-lang.org/packages/elm/html/latest/) within [`Restrictive.Ui`](Restrictive.Ui)

@docs Ui, singleton


# Create Links

_For use cases, read [the Create Links section in the State module](Restrictive-State#create-links)_

@docs toggle, goTo, bounce, filter


# Wrap the DOM

@docs Wrapper, ol, ul, keyedNode, nest


# Layout

@docs layout


### Costruct your own:

@docs removed, removable, inserted, wrap, getTemplates, arrange, concat

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed
import Html.Lazy
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout.Region as Region exposing (OrHeader(..), Region(..), withHeader)
import Restrictive.Link as State exposing (Msg(..))
import Restrictive.Ui as Ui


{-| -}
type alias Ui narrowMsg msg =
    Ui.Ui
        Region
        (HtmlList (Msg msg))
        (Wrapper narrowMsg msg)


{-| -}
type alias NarrowUi narrowMsg =
    Ui narrowMsg narrowMsg


{-| -}
singleton : List (Html msg) -> Ui narrowMsg_ msg
singleton =
    List.map (Html.map AppMsg) >> Ui.singleton



---- Create links ----


{-| Toggle a `flag` and show/hide the nested `Ui` accordingly.
-}
toggle :
    List (Html.Attribute Never)
    ->
        { flag : State.Flag
        , isInline : Bool
        , label : HtmlList msg
        }
    -> Ui narrowMsg msg
    -> Ui narrowMsg msg
toggle attrs { flag, isInline, label } =
    always
        >> Link
            (getTemplates attrs)
            { isInline = isInline
            , label = List.map (Html.map AppMsg) label
            }
            (State.toggle flag)
        >> Ui.wrap


{-| Toggle a `flag` and show/hide the nested `Ui` accordingly.
-}
filter :
    List (Html.Attribute Never)
    ->
        { category : State.Category
        , isInline : Bool
        , label : HtmlList msg
        , value : State.SearchTerm
        }
    -> (List String -> Ui narrowMsg msg)
    -> Ui narrowMsg msg
filter attrs { category, isInline, label, value } conditionalUi =
    (\linkData ->
        case linkData of
            Just (State.Filtered strs) ->
                conditionalUi strs

            Nothing ->
                conditionalUi []
    )
        |> Link
            (getTemplates attrs)
            { isInline = isInline
            , label = List.map (Html.map AppMsg) label
            }
            (State.filter [ ( category, value ) ])
        |> Ui.wrap


{-| Navigate to a `destination` and show the nested `Ui` when it's reached.
-}
goTo :
    List (Html.Attribute Never)
    ->
        { destination : ( Maybe State.Path, State.Fragment )
        , isInline : Bool
        , label : HtmlList msg
        }
    -> Ui narrowMsg msg
    -> Ui narrowMsg msg
goTo attrs { destination, isInline, label } =
    always
        >> Link
            (getTemplates attrs)
            { isInline = isInline
            , label = List.map (Html.map AppMsg) label
            }
            (State.goTo destination)
        >> Ui.wrap


{-| Navigate to `there`, and from there, back `here`.
-}
bounce :
    List (Html.Attribute Never)
    ->
        { here : ( Maybe State.Path, State.Fragment )
        , label : HtmlList msg
        , there : ( Maybe State.Path, State.Fragment )
        }
    -> Ui narrowMsg msg
    -> Ui narrowMsg msg
bounce attrs { here, label, there } =
    always
        >> Link
            (getTemplates attrs)
            { isInline = False
            , label = List.map (Html.map AppMsg) label
            }
            (State.bounce { here = here, there = there })
        >> Ui.wrap



---- Working with contingent transformations ----


{-| -}
type Wrapper narrowMsg msg
    = Node String (List (Html.Attribute (Msg msg))) (Ui narrowMsg msg)
    | Nest
        { combine :
            { makeInnerHtml :
                NarrowUi narrowMsg
                -> Maybe (HtmlList (Msg narrowMsg))
            }
            -> HtmlList (Msg msg)
        }
    | Link (State.Templates (HtmlList (Msg msg))) (State.Style (HtmlList (Msg msg))) State.Link (Maybe State.Data -> Ui narrowMsg msg)
    | Keyed (List ( String, HtmlList (Msg msg) ) -> HtmlList (Msg msg)) (List ( String, Ui narrowMsg msg ))


type alias HtmlList msg =
    List (Html msg)


applyKeyedFu : (List ( String, Html msg ) -> Html msg) -> (List ( String, HtmlList msg ) -> HtmlList msg)
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
    Keyed (applyKeyedFu (Html.Keyed.ol (List.map (Attr.map AppMsg) attrs))) >> Ui.wrap


{-| -}
ul : List (Html.Attribute msg) -> List ( String, Ui narrowMsg msg ) -> Ui narrowMsg msg
ul attrs =
    Keyed (applyKeyedFu (Html.Keyed.ul (List.map (Attr.map AppMsg) attrs))) >> Ui.wrap


{-| -}
keyedNode : String -> List (Html.Attribute msg) -> List ( String, Ui narrowMsg msg ) -> Ui narrowMsg msg
keyedNode tagName attrs =
    Keyed (applyKeyedFu (Html.Keyed.node tagName (List.map (Attr.map AppMsg) attrs))) >> Ui.wrap


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
            -> Maybe (HtmlList (Msg narrowMsg))
        }
        -> HtmlList (Msg msg)
    }
    -> Ui narrowMsg msg
nest config =
    Ui.wrap (Nest config)



----- LAYOUT -----


{-| -}
layout :
    Ui.Layout
        Region
        (HtmlList (Msg narrowMsg))
        (HtmlList (Msg msg))
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
removed : HtmlList msg -> HtmlList msg
removed =
    List.map (\a -> Html.span [ Attr.class "removed", Attr.attribute "aria-hidden" "true", Attr.tabindex -1 ] [ a ])


{-| -}
removable : HtmlList msg -> HtmlList msg
removable =
    List.map (\a -> Html.span [ Attr.class "removable" ] [ a ])


{-| -}
inserted : HtmlList msg -> HtmlList msg
inserted =
    List.map (\a -> Html.span [ Attr.class "inserted removable" ] [ a ])


{-| -}
wrap :
    Wrapper narrowMsg msg
    ->
        Ui.Wrapper
            Region
            (HtmlList (Msg narrowMsg))
            (HtmlList (Msg msg))
            (Wrapper narrowMsg narrowMsg)
            (Wrapper narrowMsg msg)
wrap wrapper =
    case wrapper of
        Node str attrs elements ->
            Ui.WrapHtml (Html.node str attrs >> List.singleton) elements

        Nest { combine } ->
            Ui.Nest
                { regions = [ Scene, Control, Info ]
                , narrowLayout = layout
                , combine = combine
                }

        Link templates style link elements ->
            Ui.Link templates style link elements

        Keyed fu list ->
            Ui.Keyed fu list


{-| -}
concat : List (List a) -> List a
concat =
    List.concat


{-| -}
arrange : Get (OrHeader Region) (HtmlList msg) -> HtmlList msg
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
getTemplates : List (Html.Attribute Never) -> State.Templates (HtmlList (Msg msg_))
getTemplates attrs =
    {-
       Field `link` expected
           `{ url : String, label : List (Html msg), isCurrent : Bool } -> HtmlList (Msg msg)`


       , found
           `{ url : String, label : Html modelMsg, isCurrent : Bool } -> List unknown`
    -}
    { link =
        \{ href, label, isCurrent } ->
            [ Html.a
                (Attr.href href
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
        \{ href, label, isChecked } ->
            [ Html.a
                (Attr.href href
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
        \{ assignments, label, isCurrent } ->
            let
                drawInput : ( State.Category, State.SearchTerm ) -> Html (Msg modelMsg)
                drawInput ( category, searchTerm ) =
                    Html.input
                        (Attr.value searchTerm
                            :: Events.onInput
                                (\newSearchTerm -> UrlCmds [ ( category, newSearchTerm ) ])
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
            in
            label ++ List.map drawInput assignments
    }
