module Ui.Layout.Aspect exposing
    ( Aspect(..)
    , all
    , inverse, subtract
    )

{-| Categorise the parts of your [Ui item](Ui)

This helps with [layouting](Ui.Layout) and [progressive disclosure](Ui.Link#toggle)

@docs Aspect

@docs all

@docs inverse, subtract

-}


{-| **Scene:** the Item's editable contents and overlays

**Control:** Toolbar, Property sheet

**Info:** Status bar, Help screen, Tooltip bubble, Snack bar

-}
type Aspect
    = Scene
    | Info
    | Control


{-| -}
all : List Aspect
all =
    [ Scene, Info, Control ]


{-|

    [ Scene, Control ]
        |> inverse
        --> [Info]

-}
inverse : List Aspect -> List Aspect
inverse comparison =
    subtract comparison all


{-|

    [ Scene, Control ]
        |> subtract [ Control ]
        --> [Scene]

    [ Scene ]
        |> subtract [ Control, Info ]
        --> [Scene]

    subtract all all
        --> []

-}
subtract : List Aspect -> List Aspect -> List Aspect
subtract whatToSubstract =
    List.filter
        (\a -> not <| List.member a whatToSubstract)
