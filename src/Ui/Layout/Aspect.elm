module Ui.Layout.Aspect exposing (Aspect(..))

{-|

@docs Aspect

-}


{-|

  - control: global toolbar or property sheet
  - info: statusbar, help screen, or tooltip bubbles
  - scene: the item's editable contents and overlays

-}
type Aspect
    = Scene
    | Control
    | Info
