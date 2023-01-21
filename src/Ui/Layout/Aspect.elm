module Ui.Layout.Aspect exposing
    ( Aspect(..)
    , intersect, inverse, subtract, isMember
    )

{-| Categorise the parts of your [Ui item](Ui)

This helps with [layouting](Ui.Layout) and [progressive disclosure](Ui.Link#toggle)

@docs Aspect

@docs intersect, inverse, subtract, isMember

-}


{-| **Scene:** the Item's editable contents and overlays

**Control:** Toolbar, Property sheet

**Info:** Status bar, Help screen, Tooltip bubble, Snack bar

-}
type Aspect
    = Scene
    | Control
    | Info


{-|

    [ Scene, Control ]
        |> inverse
        --> [Info]

-}
inverse : List Aspect -> List Aspect
inverse comparison =
    [ Scene, Control, Info ]
        |> subtract comparison


{-|

    [ Scene, Control ]
        |> subtract [ Control ]
        --> [Scene]

    [ Scene ]
        |> subtract [ Control, Info ]
        --> [Scene]

-}
subtract : List Aspect -> List Aspect -> List Aspect
subtract comparison =
    List.filter
        (\a -> not <| List.member a comparison)


{-|

    [ Scene, Control ]
        |> intersect [ Control, Info ]
        --> [Control]

    [ Scene ]
        |> intersect [ Control, Info ]
        --> []

-}
intersect : List Aspect -> List Aspect -> List Aspect
intersect comparison =
    List.filter
        (\a -> List.member a comparison)


{-| -}
isMember : List Aspect -> Aspect -> Bool
isMember list aspect =
    List.member aspect list
