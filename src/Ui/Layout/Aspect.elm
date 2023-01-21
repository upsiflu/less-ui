module Ui.Layout.Aspect exposing
    ( Aspect(..)
    , inverse, subtract
    )

{-| Categorise the parts of your [Ui item](Ui)

This helps with [layouting](Ui.Layout) and [progressive disclosure](Ui.Link#toggle)

@docs Aspect

-}


{-| **Scene:** the Item's editable contents and overlays

**Control:** Toolbar, Property sheet

**Info:** Status bar, Help screen, Tooltip bubble, Snack bar

-}
type Aspect
    = Scene
    | Control
    | Info


{-| -}
inverse : List Aspect -> List Aspect
inverse comparison =
    [ Scene, Control, Info ]
        |> subtract comparison


{-| -}
subtract : List Aspect -> List Aspect -> List Aspect
subtract comparison =
    List.filter
        (\a -> not <| List.member a comparison)
