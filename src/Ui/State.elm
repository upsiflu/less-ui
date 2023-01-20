module Ui.State exposing
    ( State, Path, Flag, Fragment, Query
    , init
    , setPath, setFragment
    , addAssignment, removeAssignments, toggleFlag, turnOnFlag
    , hasFlag
    , toUrlString
    , getFragment, getPath
    )

{-| We use the Url query to keep track of the Ui state. This makes sharing a Ui state as easy as copying the Url.

@docs State, Path, Flag, Fragment, Query


# Create

@docs init


# Map

@docs setPath, setFragment

@docs addAssignment, removeAssignments, toggleFlag, turnOnFlag


# Query

@docs hasFlag


# Deconstruct

@docs toUrlString
@docs getFragment, getPath

-}

import Set exposing (Set)
import Set.Extra as Set
import Url exposing (Url)


{-| -}
type alias State =
    Url


{-| Turning off a `Flag` renders invisible the corresponding [`Control`](Ui.Layout.Aspect) with its descendants, as well as
one-layer deep nested [`Control`s](Ui.Layout.Aspect) with their descendants.

The pattern is **progressive disclosure**.

-}
type alias Flag =
    String


{-| Paths may represent an **editing-cursor position** or **viewport**. This is up to the app to define for now.
-}
type alias Path =
    String


{-| "Graphical Web browsers typically scroll to position pages so that the top of the element
identified by the fragment id is aligned with the top of the viewport;
thus fragment identifiers are often used in tables of content and in permalinks.
The appearance of the identified element can be changed through
the `:target` CSS pseudoclass; Wikipedia uses this to highlight the selected reference.
(from Wikipedia)"
-}
type alias Fragment =
    Maybe String



---- Create ----


{-| -}
init : State -> State
init state =
    state



---- Map ----


{-| -}
setPath : Path -> State -> State
setPath path state =
    { state | path = "/" ++ path }


{-| -}
setFragment : Fragment -> State -> State
setFragment fragment state =
    { state | fragment = fragment }


{-|

    import Url

    testQuery : (State -> State) -> String -> String
    testQuery fu =
        (++) "http://localhost/?"
            >> Url.fromString
            >> Maybe.andThen (fu >> .query)
            >> Maybe.withDefault "Url.fromString or .query failed"

    "f&g&h&a=b&c=d=e"
        |> testQuery (turnOnFlag "g")
        --> "f&g&h&a=b&c=d=e"

    "a=b&c=d=e&f&g"
        |> testQuery (turnOnFlag "h")
        --> "f&g&h&a=b&c=d=e"

    "f"
        |> testQuery (turnOnFlag "")
        --> "f"


    ""
        |> testQuery (turnOnFlag "h")
        --> "h"

    ""
        |> testQuery (turnOnFlag "")
        --> ""

-}
turnOnFlag : Flag -> State -> State
turnOnFlag flag =
    if flag == "" then
        identity

    else
        mapQuery <| \q -> { q | flags = Set.insert flag q.flags }


{-|

    import Url

    testQuery : (State -> State) -> String -> String
    testQuery fu =
        (++) "http://localhost/?"
            >> Url.fromString
            >> Maybe.andThen (fu >> .query)
            >> Maybe.withDefault "Url.fromString or .query failed"

    "f&g&h&a=b&c=d=e"
        |> testQuery (toggleFlag "g")
        --> "f&h&a=b&c=d=e"

    "f&h&a=b&c=d=e"
        |> testQuery (toggleFlag "g")
        --> "f&g&h&a=b&c=d=e"

-}
toggleFlag : Flag -> State -> State
toggleFlag flag =
    if flag == "" then
        identity

    else
        mapQuery <| \q -> { q | flags = Set.toggle flag q.flags }


{-|

    import Url

    testQuery : (State -> State) -> String -> String
    testQuery fu =
        (++) "http://localhost/?"
            >> Url.fromString
            >> Maybe.andThen (fu >> .query)
            >> Maybe.withDefault "Url.fromString or .query failed"

    "f&g&h&a=b&c=d=e"
        |> testQuery (addAssignment "c" "x")
        --> "f&g&h&c=x&a=b&c=d=e"

    ""
        |> testQuery (addAssignment "" "x")
        --> "=x"

    "=x"
        |> testQuery (addAssignment "" "y")
        --> "=y&=x"

    "=y&=x"
        |> testQuery (addAssignment "" "")
        --> "=&=y&=x"

-}
addAssignment : String -> String -> State -> State
addAssignment key value =
    mapQuery <| \q -> { q | assignments = ( key, value ) :: q.assignments }


{-| -}
removeAssignments : List String -> State -> State
removeAssignments keys =
    mapQuery <|
        \q ->
            { q
                | assignments =
                    List.filter
                        (Tuple.first >> (\key -> List.member key keys |> not))
                        q.assignments
            }


mapQuery : (Query -> Query) -> State -> State
mapQuery fu state =
    { state
        | query =
            Maybe.map (toQuery >> fu >> fromQuery) state.query
    }


fromQuery : Query -> String
fromQuery query =
    Set.toList query.flags
        ++ List.map (\( k, v ) -> k ++ "=" ++ v) query.assignments
        |> String.join "&"


toQuery : String -> Query
toQuery =
    String.split "&"
        >> List.foldr
            (\entry query ->
                case String.split "=" entry of
                    [] ->
                        query

                    [ "" ] ->
                        query

                    [ flag ] ->
                        { query | flags = Set.insert flag query.flags }

                    ass :: ignment ->
                        { query | assignments = ( ass, String.join "=" ignment ) :: query.assignments }
            )
            { flags = Set.empty, assignments = [] }



---- Query ----


{-| -}
hasFlag : Flag -> Url -> Bool
hasFlag flag =
    getFlags >> List.member flag



---- Deconstruct ----


{-| -}
toUrlString : State -> String
toUrlString =
    Url.toString


{-| -}
getFragment : State -> Fragment
getFragment =
    .fragment


{-| -}
getPath : State -> Path
getPath { path } =
    String.dropLeft 1 path


{-| -}
type alias Query =
    { flags : Set Flag, assignments : List ( String, String ) }


getFlags : Url -> List Flag
getFlags =
    .query
        >> Maybe.map (toQuery >> .flags >> Set.toList)
        >> Maybe.withDefault []
