module Restrictive.Layout exposing
    ( Layout
    , list, list_, textual
    )

{-| Lay out the [`ViewModel`](Ui.Layout.ViewModel)

For choosing as set of regions for your app, consult
[the corresponding w3 WAI tutorial](https://www.w3.org/WAI/tutorials/page-structure/regions/).

Note that `Restrictive` always assumes a `Header` region.

@docs Layout


# General cases

@docs list, list_, textual

-}

import Bool.Extra exposing (ifElse)
import Restrictive.Get as Get exposing (Get)
import Restrictive.Layout.Region as Region exposing (OrHeader(..), Region(..), withHeader)
import Restrictive.State as State


{-| The layout is a rule for mapping a Ui to an html tree.
-}
type alias Layout region html attribute wrapper =
    { removed : wrapper
    , removable : wrapper
    , inserted : wrapper
    , wrap : wrapper -> html -> html
    , elements : State.Elements html attribute
    , concat : List html -> html
    , arrange : Get (OrHeader region) html -> html
    }



---- Defaults ---


{-| Demonstrates removal and insertion operations
-}
list :
    ( region, List region )
    ->
        Layout
            region
            (List ( String, element ))
            attribute
            (List ( String, element ) -> List ( String, element ))
list regions =
    { removed = List.map (\( _, v ) -> ( "-", v ))
    , removable = List.map (\( _, v ) -> ( "=", v ))
    , inserted = List.map (\( _, v ) -> ( "+", v ))
    , wrap = identity
    , elements =
        { link = \_ { url, label } -> List.map (\( _, v ) -> ( url, v )) label
        , switch = \_ { url, label } -> List.map (\( _, v ) -> ( url, v )) label
        }
    , concat = List.concat
    , arrange = Get.concatValues (Region.withHeader regions)
    }


{-| Demonstrates removal and insertion operations
-}
list_ :
    (List element -> element)
    -> Layout region element attribute ()
list_ concat =
    { removed = ()
    , removable = ()
    , inserted = ()
    , wrap = \_ -> identity
    , elements =
        { link = \_ { label } -> label
        , switch = \_ { label } -> label
        }
    , concat = concat
    , arrange = Get.listPairs >> List.map Tuple.second >> concat
    }



---- Text ----


{-| -}
textual : Layout Region String String ()
textual =
    { removed = ()
    , removable = ()
    , inserted = ()
    , wrap = \_ -> identity
    , elements =
        let
            showElement : String -> String -> List String -> String
            showElement url label attrs =
                "[" ++ url ++ "](" ++ label ++ " - " ++ String.join ", " attrs ++ ")"
        in
        { link = \attrs { url, label } -> showElement url label attrs
        , switch =
            \attrs { url, label, isChecked } ->
                showElement
                    url
                    (ifElse "☑ " "☐ " isChecked ++ label)
                    attrs
        }
    , concat = String.join ", "
    , arrange =
        Get.toListBy
            textLayout
            (withHeader Region.allRegions)
            >> String.join "\n\n\n"
    }


textLayout : Get (OrHeader Region) (String -> String)
textLayout =
    Get.fromList
        [ ( Header
          , (++) "handle: "
          )
        , ( Region Scene
          , (++) "scene"
          )
        , ( Region Control
          , (++) "control"
          )
        , ( Region Info
          , (++) "info"
          )
        ]
