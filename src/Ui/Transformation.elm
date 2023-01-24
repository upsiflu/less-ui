module Ui.Transformation exposing
    ( Transformation, neutral
    , map, mapContent
    , difference
    )

{-|

@docs Transformation, neutral

@docs map, mapContent

@docs difference

-}


{-| A Transformation is the deviation of a Ui Item from the `neutral` state.
It is defunctionalised so that different Ui states can be compared ([see `difference`](#difference)), and removals can be postponed
for animation purposes.
-}
type alias Transformation aspect content =
    { occlude : List aspect
    , appendWhere : Maybe aspect
    , appendWhat : List content
    }



---- Create ----


{-|

    { occlude = []
    , appendWhere = Nothing
    , appendWhat = []
    }

-}
neutral : Transformation aspect content
neutral =
    { occlude = []
    , appendWhere = Nothing
    , appendWhat = []
    }



---- Map ----


{-| -}
map : (content -> content2) -> Transformation aspect content -> Transformation aspect content2
map fu { occlude, appendWhere, appendWhat } =
    { occlude = occlude
    , appendWhere = appendWhere
    , appendWhat = List.map fu appendWhat
    }


{-| -}
mapContent : (b -> b) -> { t | appendWhat : b } -> { t | appendWhat : b }
mapContent fu t =
    { t | appendWhat = fu t.appendWhat }



---- Query ----


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
    Transformation aspect content
    -> Transformation aspect content
    -> { addition : Transformation aspect content, removal : Transformation aspect content, unchanged : Transformation aspect content }
difference current previous =
    let
        ( addedContent, removedContent, unchangedContent ) =
            if current.appendWhere == previous.appendWhere then
                ( [], [], current.appendWhat )

            else
                ( current.appendWhat, previous.appendWhat, [] )

        intersect : List a -> List a -> List a
        intersect =
            List.filter << isMemberOf

        subtract : List a -> List a -> List a
        subtract negative =
            List.filter (not << isMemberOf negative)

        isMemberOf : List a -> a -> Bool
        isMemberOf list a =
            List.member a list
    in
    { addition =
        { occlude =
            subtract previous.occlude current.occlude
        , appendWhere =
            current.appendWhere
        , appendWhat =
            addedContent
        }
    , removal =
        { occlude =
            subtract current.occlude previous.occlude
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
