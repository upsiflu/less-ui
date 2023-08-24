module Restrictive.Get exposing
    ( Get
    , empty, singleton
    , insert, updateAt, updateWhere
    , addValue, addWithDefault, maybeAdd
    , remove
    , consList, addList
    , map, map2, map2WithDefaults, map2Value, filter
    , mapByKey
    , append, concat, concatMap, concatBy
    , orElse, superimpose
    , andMap
    , member, andGet
    , Mutation(..), mutation
    , toList, toListBy, keys, values, fromList, fromListBy
    , listPairs
    , consLists, concatLists, concatValues
    , sequence
    , get
    )

{-| Fallible mapping from `key` to `value`

@docs Get


# Create

@docs empty, singleton


# Associate values

@docs insert, updateAt, updateWhere
@docs addValue, addWithDefault, maybeAdd
@docs remove


### Add to `Get (List a)`

@docs consList, addList


# Modify

@docs map, map2, map2WithDefaults, map2Value, filter


### Map with respect to the key

@docs mapByKey


# Compose


### `Get (List a)`

@docs append, concat, concatMap, concatBy


### Get either value

@docs orElse, superimpose


### Chain

@docs andMap


# Query

@docs member, andGet


### Mutations

@docs Mutation, mutation


# Lists

Known keys:

@docs toList, toListBy, keys, values, fromList, fromListBy

All keys:

@docs listPairs


### Convenience functions for `Get (List a)`

@docs consLists, concatLists, concatValues


### Folding

@docs sequence


# Apply

@docs get

-}

import AssocList as Dict exposing (Dict)
import Bool.Extra as Bool
import Maybe.Extra as Maybe


{-| -}
type alias Get key a =
    Dict key a



---- CREATE ----


{-| in any case, return Nothing
-}
empty : Get key a
empty =
    Dict.empty


{-| -}
singleton : key -> a -> Get key a
singleton =
    Dict.singleton



---- ASSOCIATE VALUES ----


{-| insert at key, combining with existing value

    import Restrictive.Layout.Region exposing (Region(..))

    singleton Control [3]
        |> maybeAdd (++) Scene [1, 2]
        |> get Scene
            --> Nothing

    singleton Scene "b"
        |> maybeAdd (++) Scene "a"
        |> get Scene
            --> Just "ab"

    singleton Control 4
        |> maybeAdd (+) Control 5
        |> get Control
            --> Just 9

-}
maybeAdd : (a -> b -> c) -> key -> a -> Get key b -> Get key c
maybeAdd =
    map2 >> composeWithTwoParameters singleton


{-| Define a default so that the composition can't fail

    addWithDefault 0 (+) () 1 (singleton () 2)
        |> get ()
        --> Just 3

    addWithDefault 0 (+) () 1 empty
        |> get ()
        --> Just 1

-}
addWithDefault : a -> (a -> a -> b) -> key -> a -> Get key a -> Get key b
addWithDefault default =
    map2WithDefaults default
        >> composeWithTwoParameters singleton


{-| Consider `Nothing` in the `add` composer so that the composition can't fail

    import Restrictive.Layout.Region exposing (Region(..))


    mapLists : (List a -> List a -> List a) -> Maybe (List a) -> Maybe (List a) -> List a
    mapLists fu ma mb =
        fu (Maybe.withDefault [] ma) (Maybe.withDefault [] mb)


    singleton Control [3]
        |> addValue (mapLists (++)) Scene [1, 2]
        |> get Scene
            --> Just [1, 2]

    singleton Control [3]
        |> addValue (mapLists (++)) Scene [1, 2]
        |> get Control
            --> Just [3]

    singleton Control [3]
        |> addValue (mapLists (++)) Scene [1, 2]
        |> addValue (mapLists (++)) Scene [0]
        |> get Scene
            --> Just [0, 1, 2]

    empty
        |> addValue (Maybe.map2 (+)) Scene 1
        |> get Scene
            --> Just Nothing

-}
addValue : (Maybe a -> Maybe b -> c) -> key -> a -> Get key b -> Get key c
addValue =
    map2Value
        >> composeWithTwoParameters singleton


{-| -}
remove : key -> Get key a -> Get key a
remove =
    Dict.remove


mapLists : (List a -> List a -> List a) -> Maybe (List a) -> Maybe (List a) -> List a
mapLists fu ma mb =
    fu (Maybe.withDefault [] ma) (Maybe.withDefault [] mb)


{-| Update the value associated with a specified key, if present

    import Restrictive.Layout.Region exposing (Region(..))

    singleton Scene "a"
        |> updateAt Scene String.toUpper
        |> get Scene

    --> Just "A"

    singleton Scene "a"
        |> updateAt Control String.toUpper
        |> get Scene

    --> Just "a"

    empty
        |> updateAt Scene ((+) 1)
        |> get Scene

    --> Nothing

-}
updateAt :
    key
    -> (a -> a)
    -> Get key a
    -> Get key a
updateAt key fu =
    mapByKey ((==) key >> Bool.ifElse fu identity)


{-|

    import Restrictive.Layout.Region exposing (Region(..))

    fromList [ (Scene, 1), (Control, 1)]
        |> updateWhere  ( (==) Control )
            negate
        |> get Control
        --> Just (-1)

    singleton Control 1
        |> updateWhere ( (==) Scene )
            negate
        |> get Control
        --> Just (1)

-}
updateWhere : (key -> Bool) -> (a -> a) -> Get key a -> Get key a
updateWhere predicate fu =
    Dict.map
        (\k a ->
            if predicate k then
                fu a

            else
                a
        )


{-|

    consList () 1 (singleton () [2, 3])
        |> get ()
        --> Just [1, 2, 3]

Implicitly creates a list if key has no value yet:

    consList () "a" empty
        |> get ()
            --> Just ["a"]

-}
consList : key -> a -> Get key (List a) -> Get key (List a)
consList =
    addValue
        (\ma mb ->
            Maybe.unwrap [] List.singleton ma
                ++ Maybe.withDefault [] mb
        )


{-| `addWithDefault [] (++)`

Note that `fromList` only accepts the first input per key:

    import Restrictive.Layout.Region exposing (Region(..))

    fromList [(Control, [1]), (Control, [2])]
        |> addList Control [3, 4]
        |> get Control
        --> Just [1, 3, 4]

    fromList [(Control, [1]), (Control, [2])]
        |> addList Scene [3, 4]
        |> get Control
        --> Just [1]

-}
addList : key -> List a -> Get key (List a) -> Get key (List a)
addList =
    addWithDefault [] (\x y -> y ++ x)



---- MAP ----


{-| compose a function behind the result

    map negate (singleton () 1)
        |> get ()
        --> Just (-1)

-}
map : (a -> b) -> Get key a -> Get key b
map fu =
    Dict.map (\_ -> fu)



-- {-| Maps to a potentially narrower key-space, preserving values
--     singleton "1" "One"
--         |> filterMapKey String.toInt
--         |> get 1
--         --> "One"
-- -}
-- filterMapKey : (k -> Maybe l) -> Get k a -> Get l a
-- filterMapKey narrow =
--     filterMap <|
--         \k a -> narrow k |> Maybe.map (\l -> ( l, a ))
-- {-| Maps to a potentially narrower value-space, preserving values
-- -}
-- filterMapValue : (a -> Maybe b) -> Get k a -> Get k b
-- filterMapValue narrow =
--     filterMap <|
--         \k a -> narrow a |> Maybe.map (\b -> ( k, b ))
-- {-| Maps to a potentially narrower key-value-space
--     singleton 1 2
--         |> insert 2 4
--         |> filterMap
--             (\key value ->
--                 if key % 2 == 0 then
--                     Just key
--                 else
--                     Nothing
--             )
-- -}
-- filterMap : (k -> a -> Maybe ( l, b )) -> Get k a -> Get l b
-- filterMap narrow =
--     Dict.foldl
--         (\k a ->
--             case narrow k a of
--                 Just ( l, b ) ->
--                     Dict.insert l b
--                 _ ->
--                     identity
--         )
--         Dict.empty


{-| Apply a sequence of operations on a `Get`:

    import Restrictive.Layout.Region exposing (Region(..))

    singleton Scene 1
        |> sequence
            [ map ((*) 2)
            , filter (modBy 2 >> (==) 0)
            , map ((+) 1)
            ]
        |> get Scene
        --> Just 3

-}
sequence : List (Get key a -> Get key a) -> Get key a -> Get key a
sequence operations getA =
    List.foldl
        (<|)
        getA
        operations


{-| only get a value if the condition holds

    import Restrictive.Layout.Region exposing (Region(..))

    singleton Scene 1
        |> insert Control 2
        |> insert Info 1
        |> filter ((==) 1)
        |> toList [Scene, Info, Control]
        --> [(Scene, 1), (Info, 1)]

-}
filter : (a -> Bool) -> Get key a -> Get key a
filter predicate =
    Dict.filter (\_ -> predicate)



-- {-| compose a function behind the result, preserving the 'Maybe' value.
--    `mapValue = (<<)`
--    You can use this function to leverage mapping functions from the `Maybe` and `Maybe.Extra` library.
--        import Maybe
--        import Restrictive.Layout.Region exposing (Region(..))
--        withDefault : a -> Maybe a -> Maybe a
--        withDefault a =
--            Maybe.withDefault a >> Just
--        fromList [ (Scene, 1), (Control, 2)]
--            |> mapValue (withDefault 4)
--            |> toList [Scene, Control, Info]
--            --> [ (Scene, 1), (Control, 2), (Info, 4) ]
-- -}
-- mapValue : (Maybe a -> Maybe b) -> Get key a -> Get key b
-- mapValue =
--     (<<)


{-| parametrize map over key.
Same as andMapFlat!
-}
mapByKey : (key -> a -> b) -> Get key a -> Get key b
mapByKey =
    Dict.map



{- -}
-- mapValueByKey : (key -> Maybe a -> Maybe b) -> Get key a -> Get key b
-- mapValueByKey fu getA =
--     Dict.map
--         (\k -> fu k)
--     getA key
--         |> fu key


{-| Compose a function behind the results of two `Get`s,
both with the identical key.
Note that if any of the two values is Nothing, the other is ignored.

    map2 (::) (singleton () 1) (singleton () [2, 3])
        |> get ()
        --> Just [1, 2, 3]

-}
map2 : (a -> b -> c) -> Get key a -> Get key b -> Get key c
map2 fu =
    merge
        (\_ _ -> identity)
        (\k a b -> insert k (fu a b))
        (\_ _ -> identity)


{-| -}
insert : k -> a -> Get k a -> Get k a
insert =
    Dict.insert


{-| -}
map2WithDefaults : a -> (a -> a -> b) -> Get key a -> Get key a -> Get key b
map2WithDefaults default composer =
    map2Value (\m1 m2 -> composer (Maybe.withDefault default m1) (Maybe.withDefault default m2))


{-| Never fail
-}
map2Value : (Maybe a -> Maybe b -> c) -> Get key a -> Get key b -> Get key c
map2Value fu =
    merge
        (\k a ->
            fu (Just a) Nothing
                |> insert k
        )
        (\k a b ->
            Just b
                |> fu (Just a)
                |> insert k
        )
        (\k b ->
            Just b
                |> fu Nothing
                |> insert k
        )



---- COMPOSE ----


{-| Interprets Nothing as [].

Difference to `addList`: Both lists to append can be Nothing.

    import Restrictive.Layout.Region exposing (Region(..))

    singleton Scene [1]
        |> append (singleton Scene [2, 3])
        |> get Scene
            --> Just [2, 3, 1]

-}
append : Get key (List a) -> Get key (List a) -> Get key (List a)
append =
    map2Value (mapLists (++))


{-| `concat = List.foldl merge empty`
-}
concat : List (Get key (List a)) -> Get key (List a)
concat =
    List.foldr append empty


{-| Merge the results of `fu`
-}
concatMap : (a -> Get key (List a)) -> List a -> Get key (List a)
concatMap fu =
    List.map fu >> concat


{-| Concatenate the values of several `Get`s with a custom concatenator

    [ singleton () "Hell",  singleton () "o" ]
        |> concatBy ( String.join "; " )
        |> get ()
        --> Just "Hell; o"

-}
concatBy : (List a -> a) -> List (Get key a) -> Get key a
concatBy concatenator =
    List.foldr
        (merge
            insert
            (\k a b -> insert k (concatenator [ a, b ]))
            insert
        )
        empty


{-| -}
merge :
    (k -> a -> Get k c -> Get k c)
    -> (k -> a -> b -> Get k c -> Get k c)
    -> (k -> b -> Get k c -> Get k c)
    -> Get k a
    -> Get k b
    -> Get k c
merge ac abc bc getA getB =
    Dict.merge ac abc bc getA getB Dict.empty



-- List.map (get key) list
--     |> Maybe.values
--     |> concatenator
--     |> Just


{-| Return the first value only if the second fails, using the same key.

    import Restrictive.Layout.Region exposing (Region(..))

    let
        getMy =
            empty
                |> orElse (singleton Info "b")
                |> orElse (singleton Scene "a")
                |> orElse (singleton Scene "c")
    in
    (get Scene getMy , get Control getMy )
    --> ( Just "a",  Nothing)

-}
orElse : Get key a -> Get key a -> Get key a
orElse =
    merge
        insert
        (\key _ first -> insert key first)
        insert


{-| This is `orElse` with the parameters flipped
-}
superimpose : Get key a -> Get key a -> Get key a
superimpose first second =
    orElse second first



----


{-| -}
andGet : key -> Maybe (Get key a) -> Maybe a
andGet =
    get >> Maybe.andThen


{-| Map the result of the first `get` to the next `get`.
Nice for pipelining. Inspired by the Maybe.Extra package.
-}
andMap : Get key a -> Get key (a -> b) -> Get key b
andMap =
    merge
        (\_ _ -> identity)
        (\k a ab -> insert k (ab a))
        (\_ _ -> identity)



---- QUERY ----


{-| Example:

    import Restrictive.Layout.Region exposing (Region(..))

    singleton Scene 1
        |> member Scene --> True

    singleton Control 1
        |> member Scene -> False

-}
member : key -> Get key a -> Bool
member =
    composeWithTwoParameters get Maybe.isJust


{-| -}
get : key -> Get key value -> Maybe value
get =
    Dict.get


{-| `Substitution a b`: b was substituted by a
-}
type Mutation a
    = Substitution { current : a, previous : a }
    | Insertion a
    | Deletion a
    | Protraction a


{-| Compare the current with an optional previous `Get` at each key and return the [`Mutation`s](Mutation).

    mutation (==) {current = singleton () 1, previous = Just (singleton () 2)}
        |> get ()
        --> Just (Substitution { current = 1, previous = 2})

    mutation (==) {current = singleton () 1, previous = Just (empty)}
        |> get ()
        --> Just (Insertion 1)

    mutation (==) {current = empty, previous = Just (singleton () 3)}
        |> get ()
        --> Just (Deletion 3)

    mutation (==) {current = singleton () 4, previous = Just (singleton () 4)}
        |> get ()
        --> Just (Protraction 4)

Note that in this implementation `previous` implicitly defaults to `empty`:

    mutation (==) {current = singleton () 5, previous = Nothing}
        |> get ()
        --> Just (Insertion 5)


    mutation (==) {current = empty, previous = Nothing}
        |> get ()
        --> Nothing

To avoid comparing functions, please supply a `predicate` for safe equality. It will be used to discern Protraction and Substitution.
For example, if you are comparing lists of functions, you can supply `(\l0 l1 -> List.length l0 == List.length l1)`

    {current = singleton () [(+), (*)], previous = Just (singleton () [(+), (-)])}
        |> mutation (\l0 l1 -> List.length l0 == List.length l1)
        |> get ()
         -> Just (Protraction ...)

-}
mutation : (a -> a -> Bool) -> { current : Get key a, previous : Maybe (Get key a) } -> Get key (Mutation a)
mutation predicate { current, previous } =
    -- (k -> a -> result -> result)
    -- -> (k -> a -> b -> result -> result)
    -- -> (k -> b -> result -> result)
    -- -> Dict k a
    -- -> Dict k b
    -- -> result
    -- -> result
    merge
        (\k currentValue -> insert k (Insertion currentValue))
        (\k currentValue previousValue ->
            insert k <|
                if predicate currentValue previousValue then
                    Protraction previousValue

                else
                    Substitution { current = currentValue, previous = previousValue }
        )
        (\k previousValue -> insert k (Deletion previousValue))
        current
        (Maybe.withDefault empty previous)



-- {-| -}
-- listMutation : { current : Get key (List a), previous : Maybe (Get key (List a)) } -> Get key (Mutation (List a))
-- listMutation { current, previous } =
--     -- (k -> a -> result -> result)
--     -- -> (k -> a -> b -> result -> result)
--     -- -> (k -> b -> result -> result)
--     -- -> Dict k a
--     -- -> Dict k b
--     -- -> result
--     -- -> result
--     merge
--         (\k currentValue -> insert k (Insertion currentValue))
--         (\k currentValue previousValue ->
--             insert k <|
--                 if List.length currentValue == List.length previousValue then
--                     Protraction previousValue
--                 else
--                     Substitution { current = currentValue, previous = previousValue }
--         )
--         (\k previousValue -> insert k (Deletion previousValue))
--         current
--         (Maybe.withDefault empty previous)
-- {-| Omits equality checks for `Protraction`s
-- (Useful if you `get` objects with functions)
-- -}
-- simpleMutation : { current : Get key a, previous : Maybe (Get key a) } -> Get key (Mutation a)
-- simpleMutation =
--     mutation (\_ _ -> False)
---- SERIALISE TO LIST ----


{-|

    import Restrictive.Layout.Region exposing (Region(..))

    singleton Scene "Hello"
        |> toList [Scene, Scene]
        --> [(Scene, "Hello"), (Scene, "Hello")]

-}
toList : List key -> Get key a -> List ( key, a )
toList list getA =
    List.foldr
        (\key ->
            get key getA
                |> Maybe.map (Tuple.pair key)
                |> Maybe.cons
        )
        []
        list


{-| `orAdd`s from the list but skips duplicate keys.
`fromList = fromListWith orAdd`

If you want to modify the behavior on duplicate keys, use `fromListWith`
or `consList`.

    import Restrictive.Layout.Region exposing (Region(..))

    fromList [(Scene, "Scene 1"), (Scene, "Scene 2"), (Control, "C")]
        |> toList [Scene, Control]
        --> [(Scene, "Scene 1"), (Control, "C")]

-}
fromList : List ( key, a ) -> Get key a
fromList =
    --fromListWith : (Region -> a -> Get b -> Get b) -> List ( Region, a ) -> Get b
    fromListBy orInsert


{-| avoids collisions
-}
orInsert : key -> a -> Get key a -> Get key a
orInsert key a get1 =
    case get key get1 of
        Just _ ->
            get1

        Nothing ->
            insert key a get1


{-| -}
toListBy : Get key (a -> b) -> List key -> Get key a -> List b
toListBy getAB keys_ getA =
    getAB
        |> andMap getA
        |> Dict.toList
        |> List.filterMap
            (\( key, value ) ->
                if List.member key keys_ then
                    Just value

                else
                    Nothing
            )


{-|

    values [ 0, 1 ] (singleton 1 ()) --> [()]

-}
values : List key -> Get key a -> List a
values list getA =
    List.foldr (\k -> Maybe.cons (get k getA)) [] list


{-| -}
concatValues : List key -> Get key (List a) -> List a
concatValues list =
    values list >> List.concat


{-| -}
keys : List key -> Get key a -> List key
keys list getA =
    List.foldr
        (\key ->
            get key getA
                |> Maybe.unwrap identity (\_ -> (::) key)
        )
        []
        list


{-| `concatLists = fromListBy addList`
-}
concatLists : List ( key, List a ) -> Get key (List a)
concatLists =
    fromListBy addList


{-| `consLists = fromListBy consList`
-}
consLists : List ( key, a ) -> Get key (List a)
consLists =
    fromListBy consList


{-| -}
fromListBy : (key -> a -> Get key b -> Get key b) -> List ( key, a ) -> Get key b
fromListBy fu =
    List.map
        (\( key, a ) -> fu key a)
        >> (\operations -> sequence operations empty)



---- Helpers ----
-- {-| This is a strangely important function that deserves an important name.
-- `mapParameter a = a >> (<<)`
-- `mapParameter a b = (|>) >> (a b)`
-- `mapParameter a b c = c >> (a b)`
-- `mapParameter a b c d = (a b (c d))`
-- -}
-- mapParameter :
--     (a
--      -> (b -> c)
--     )
--     ->
--         (a
--          -> ((input -> b) -> (input -> c))
--         )
-- mapParameter function =
--     let
--         composeRightToLeft : (b -> c) -> (input -> b) -> (input -> c)
--         composeRightToLeft =
--             (<<)
--     in
--     function >> composeRightToLeft


listPairs : Dict k v -> List ( k, v )
listPairs =
    Dict.toList


composeWithTwoParameters : (a -> b -> c) -> (c -> d) -> a -> b -> d
composeWithTwoParameters function0 function1 a =
    -- compose left-to-right with two parameters
    function0 a >> function1
