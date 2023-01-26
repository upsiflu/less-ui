module Restrictive.Transformation exposing
    ( Transformation, neutral
    , map, mapContent
    , difference
    , OrAll(..), OrHeader(..)
    )

{-|

@docs Transformation, neutral

@docs map, mapContent

@docs difference

@docs OrAll, OrHeader

-}


{-| A Transformation is the deviation of a Ui Item from the `neutral` state.
It is defunctionalised so that different Ui states can be compared ([see `difference`](#difference)), and removals can be postponed
for animation purposes.
-}
type alias Transformation region content =
    { occlude : OrAll region
    , appendWhere : OrHeader region
    , appendWhat : List content
    }


{-| Work with a set of regions that can be "full".
-}
type OrAll region
    = All
    | Some (List region)
    | AllExcept (List region)


{-| Extend a sum type by a `Header` constructor. Every app in `Restrictive` has a Header.
-}
type OrHeader region
    = Header
    | Region region



---- Create ----


{-|

    { occlude = []
    , appendWhere = Header
    , appendWhat = []
    }

-}
neutral : Transformation region content
neutral =
    { occlude = Some []
    , appendWhere = Header
    , appendWhat = []
    }



---- Map ----


{-| -}
map : (content -> content2) -> Transformation region content -> Transformation region content2
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
    Transformation region content
    -> Transformation region content
    -> { addition : Transformation region content, removal : Transformation region content, unchanged : Transformation region content }
difference current previous =
    let
        ( addedContent, removedContent, unchangedContent ) =
            if current.appendWhere == previous.appendWhere then
                ( [], [], current.appendWhat )

            else
                ( current.appendWhat, previous.appendWhat, [] )

        intersect : OrAll a -> OrAll a -> OrAll a
        intersect a b =
            case a of
                All ->
                    b

                Some list ->
                    Some (List.filter (isMemberOf b) list)

                AllExcept _ ->
                    intersect (negate a) b

        negate : OrAll a -> OrAll a
        negate orAll =
            case orAll of
                All ->
                    Some []

                AllExcept list ->
                    Some list

                Some [] ->
                    All

                Some list ->
                    AllExcept list

        subtract : OrAll a -> OrAll a -> OrAll a
        subtract =
            negate >> intersect

        isMemberOf : OrAll a -> a -> Bool
        isMemberOf orAll a =
            case orAll of
                Some list ->
                    List.member a list

                All ->
                    True

                AllExcept list ->
                    not (List.member a list)
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
