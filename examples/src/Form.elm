module Form exposing (..)

import Control


type alias Customer =
    { name : String
    , age : Int
    }


customerControl =
    Control.record (\name age -> { name = name, age = age })
        |> Control.field .name (Control.string |> Control.label "Name")
        |> Control.field .age (Control.int |> Control.label "Age")
        |> Control.endRecord


main =
    Control.sandbox
        { control = customerControl
        , outputToString = Debug.toString
        }
