module Ui.Transformation exposing
    ( Transformation, neutral
    , mapContent
    , difference
    )

{-|

@docs Transformation, neutral

@docs mapContent

@docs difference

-}

import Ui.Layout.Aspect exposing (Aspect, intersect, subtract)
import Ui.Layout.ViewModel exposing (Keyed)


{-| Note that an `Aspect` of `Nothing` implies a global position.
-}
type alias Transformation content =
    { occlude : List Aspect
    , appendWhere : Maybe Aspect
    , appendWhat : List (Keyed content)
    }



---- Create ----


{-|

    { occlude = []
    , appendWhere = Nothing
    , appendWhat = []
    }

-}
neutral : Transformation content
neutral =
    { occlude = []
    , appendWhere = Nothing
    , appendWhat = []
    }



---- Query ----


{-| -}
mapContent : (b -> b) -> { t | appendWhat : b } -> { t | appendWhat : b }
mapContent fu t =
    { t | appendWhat = fu t.appendWhat }


{-|

    import Ui.Layout.Aspect as Aspect exposing (Aspect(..))

    current : Transformation Int
    current =
        { occlude = [ Scene ]
        , appendWhere = Just Scene
        , appendWhat = [("current", 1)]
        }

    previous : Transformation Int
    previous =
        { occlude = [ Scene ]
        , appendWhere = Just Control
        , appendWhat = [("previous", 2)]
        }

    difference current previous
    -->     { addition =
    -->         { occlude = []
    -->         , appendWhere = Just Scene
    -->         , appendWhat = [("current", 1)]
    -->         }
    -->     , removal =
    -->         { occlude = []
    -->         , appendWhere = Just Control
    -->         , appendWhat = [("previous", 2)]
    -->         }
    -->     , unchanged =
    -->         { occlude = [ Scene ]
    -->         , appendWhere = Just Scene
    -->         , appendWhat = []
    -->         }
    -->     }

-}
difference :
    Transformation a
    -> Transformation a
    -> { addition : Transformation a, removal : Transformation a, unchanged : Transformation a }
difference current previous =
    let
        ( addedContent, removedContent, unchangedContent ) =
            if current.appendWhere == previous.appendWhere then
                ( [], [], current.appendWhat )

            else
                ( current.appendWhat, previous.appendWhat, [] )
    in
    { addition =
        { occlude =
            current.occlude
                |> subtract previous.occlude
        , appendWhere =
            current.appendWhere
        , appendWhat =
            addedContent
        }
    , removal =
        { occlude =
            previous.occlude
                |> subtract current.occlude
        , appendWhere =
            previous.appendWhere
        , appendWhat =
            removedContent
        }
    , unchanged =
        { occlude =
            intersect current.occlude previous.occlude
        , appendWhere =
            current.appendWhere
        , appendWhat =
            unchangedContent
        }
    }
