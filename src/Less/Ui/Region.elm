module Less.Ui.Region exposing
    ( Region(..)
    , allRegions
    , subtract
    , OrHeader(..)
    , isMember, justRegion, orHeader
    , OrAll(..)
    , intersect, isMemberOf, negate, withHeader
    )

{-| Categorise the parts of your [Ui item](Less-Ui)

Note: Much or all of this module is slated for removal.

This helps with [layouting](Less-Ui#Layout) and [progressive disclosure](Less-Ui-Html#toggle)

@docs Region

@docs allRegions

@docs subtract


# Can be some region or the Header

@docs OrHeader

@docs isMember, justRegion, orHeader


# List that can be negative

@docs OrAll

@docs intersect, isMemberOf, negate, withHeader

-}


{-| **Scene:** the Item's editable contents and overlays

**Control:** Toolbar, Property sheet

**Info:** Status bar, Help screen, Tooltip bubble, Snack bar

-}
type Region
    = Scene
    | Info
    | Control


{-| -}
allRegions : ( Region, List Region )
allRegions =
    ( Scene, [ Info, Control ] )


{-| Work with a set of regions that can be "full".
-}
type OrAll region
    = All
    | Some (List region)
    | AllExcept (List region)


{-| Extend a sum type by a `Header` constructor. Every app in `Less` has a Header.
-}
type OrHeader region
    = Header
    | Region region


{-|

    withHeader ( 0, [ 1, 2 ] ) --> [Header, Region 0, Region 1, Region 2]

-}
withHeader : ( region, List region ) -> List (OrHeader region)
withHeader regions =
    Header :: List.map Region (cons regions)


{-|

    orHeader Nothing --> Header

    orHeader (Just Scene) --> Region Scene

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

    justRegion (Region Scene) --> Just Scene

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


cons : ( a, List a ) -> List a
cons ( x, xs ) =
    x :: xs
