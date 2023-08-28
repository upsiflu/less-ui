module Restrictive.Layout.Html.Ui exposing (Ui, StatelessUi)

{-| Shorter type signatures

@docs Ui, StatelessUi

-}

import Html exposing (Html)
import Restrictive.Layout.Html as Html
import Restrictive.Layout.Region exposing (Region)
import Restrictive.Ui


{-| -}
type alias Ui msg =
    Restrictive.Ui.Ui
        Region
        (List (Html msg))
        (Html.Attribute Never)
        (Html.Wrapper msg)


{-| -}
type alias StatelessUi =
    Restrictive.Ui.Ui
        Region
        (List (Html Never))
        (Html.Attribute Never)
        (Html.Wrapper Never)
