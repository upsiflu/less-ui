module Ui.Layout.Aspect exposing (Aspect(..))

{-| Categorise the parts of your [Ui item](Ui)

This helps with [layouting](Ui.Layout)

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
