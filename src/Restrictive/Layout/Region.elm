module Restrictive.Layout.Region exposing
    ( Aspect(..)
    , allAspects
    , inverse, subtract
    , OrHeader(..)
    , isMember, justRegion, orHeader
    , OrAll(..)
    , intersect, isMemberOf, negate, subtract_, withHeader
    )

{-| Categorise the parts of your [Ui item](Ui)

This helps with [layouting](Ui.Layout) and [progressive disclosure](Ui.Link#toggle)

@docs Aspect

@docs allAspects

@docs inverse, subtract


# Can be some region or the Header

@docs OrHeader

@docs isMember, justRegion, orHeader


# List that can be negative

@docs OrAll

@docs intersect, isMemberOf, negate, subtract_, withHeader

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
allAspects : List Aspect
allAspects =
    [ Scene, Info, Control ]


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


{-|

    withHeader [ 1, 2 ] --> [Header, Region 1, Region 2]

-}
withHeader : List region -> List (OrHeader region)
withHeader list =
    Header :: List.map Region list


{-|

    orHeader Nothing --> Header

    OrHeader (Just r) --> Region r

-}
orHeader : Maybe a -> OrHeader a
orHeader maybeA =
    case maybeA of
        Just a ->
            Region a

        Nothing ->
            Header


{-|

    justRegion Header --> Nothing

    justRegion (Region r) --> Just r

-}
justRegion : OrHeader a -> Maybe a
justRegion orHeader_ =
    case orHeader_ of
        Header ->
            Nothing

        Region r ->
            Just r


{-| -}
isMember : OrAll key -> key -> Bool
isMember orAll_ key =
    case orAll_ of
        All ->
            True

        Some list ->
            List.member key list

        AllExcept list ->
            not (List.member key list)


{-| -}
intersect : OrAll a -> OrAll a -> OrAll a
intersect a b =
    case a of
        All ->
            b

        Some list ->
            Some (List.filter (isMemberOf b) list)

        AllExcept _ ->
            intersect (negate a) b


{-| -}
isMemberOf : OrAll a -> a -> Bool
isMemberOf orAll a =
    case orAll of
        All ->
            True

        Some list ->
            List.member a list

        AllExcept list ->
            not (List.member a list)


{-| -}
subtract : OrAll a -> OrAll a -> OrAll a
subtract =
    negate >> intersect


{-| -}
negate : OrAll a -> OrAll a
negate orAll =
    case orAll of
        All ->
            Some []

        Some [] ->
            All

        Some list ->
            AllExcept list

        AllExcept list ->
            Some list


{-|

    [ Scene, Control ]
        |> inverse
        --> [Info]

-}
inverse : List Aspect -> List Aspect
inverse comparison =
    subtract_ comparison allAspects


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
subtract_ : List Aspect -> List Aspect -> List Aspect
subtract_ whatToSubstract =
    List.filter
        (\a -> not <| List.member a whatToSubstract)