module Ui.Get exposing
    ( Get
    , empty, full, singleton
    , orAdd, insert, update, updateValue
    , addValue, addWithDefault, maybeAdd
    , consList, addList
    , map, mapValue, map2, map2WithDefaults, map2Value, filter
    , mapByKey, mapValueByKey, map2ByKey
    , concat
    , join, unlockInner, unlockOuter
    , orElse
    , andThen, andThenFlat, andThenMaybe, andMap, andMapFlat
    , member, get, andGet, withDefault
    , toList, toListBy, keys, values, fromList, fromListBy
    , consLists, concatLists
    , sequence
    )

{-| This is a very simple dictionary type with `Aspect` as key.

@docs Get


# Create

@docs empty, full, singleton


# Associate values

@docs orAdd, insert, update, updateValue
@docs addValue, addWithDefault, maybeAdd


### Add to `Get (List a)`

@docs consList, addList


# Map

@docs map, mapValue, map2, map2WithDefaults, map2Value, filter


### Map with respect to the key

@docs mapByKey, mapValueByKey, map2ByKey


### Map `Get (List a)`

@docs concat


### Map `Get (Get a)`

@docs join, unlockInner, unlockOuter


# Compose


### Or

@docs orElse


### Chain

@docs andThen, andThenFlat, andThenMaybe, andMap, andMapFlat


# Query

@docs member, get, andGet, withDefault


# Lists

@docs toList, toListBy, keys, values, fromList, fromListBy


### Convenience functions for `Get (List a)`

@docs consLists, concatLists


### Folding

@docs sequence

-}

import Bool.Extra as Bool
import Maybe.Extra as Maybe
import Ui.Layout.Aspect exposing (Aspect)


{-| -}
type alias Get a =
    Aspect -> Maybe a



-- CREATE


{-| in any case, return Nothing
-}
empty : Get a
empty =
    always Nothing


{-| in any case, return `a`

    import Ui.Layout.Aspect exposing (Aspect(..))

    full 1
        |> get Scene
        --> Just 1

-}
full : a -> Get a
full =
    always << Just


{-| -}
singleton : Aspect -> a -> Get a
singleton key a =
    (==) key >> Bool.toMaybe a



-- ASSOCIATE VALUES


{-| insert at key if not yet populated

    import Ui.Layout.Aspect exposing (Aspect(..))

    singleton Scene "a"
        |> orAdd Scene "b"
        |> orAdd Control "c"
        |> orAdd Control "d" -- ignored because Control has been populated
        |> get Control
            --> Just "c"

-}
orAdd : Aspect -> a -> Get a -> Get a
orAdd key =
    Just >> Maybe.orElse >> updateValue key


{-| insert at key, overwriting existing value

    import Ui.Layout.Aspect exposing (Aspect(..))

    singleton Scene 1
        |> insert Scene 2
        |> get Scene
            --> Just 2

    singleton Control 1
        |> insert Scene 2
        |> get Scene
            --> Just 2

-}
insert : Aspect -> a -> Get a -> Get a
insert key =
    Just >> always >> updateValue key


{-| insert at key, combining with existing value

    import Ui.Layout.Aspect exposing (Aspect(..))

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
maybeAdd : (a -> b -> c) -> Aspect -> a -> Get b -> Get c
maybeAdd composer =
    singleton
        >> (<<) (map2 composer)


{-| Define a default so that the composition can't fail

    import Ui.Layout.Aspect exposing (Aspect(..))

    addWithDefault 0 (+) Scene 1 (full 2)
        |> get Scene
        --> Just 3

    addWithDefault 0 (+) Scene 1 empty
        |> get Scene
        --> Just 1

-}
addWithDefault : a -> (a -> a -> b) -> Aspect -> a -> Get a -> Get b
addWithDefault default composer =
    singleton >> (<<) (map2WithDefaults default composer)


{-| Consider `Nothing` in the `add` composer so that the composition can't fail

    import Ui.Layout.Aspect exposing (Aspect(..))


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

    full 1
        |> addValue (Maybe.map2 (+)) Scene 1
        |> get Scene
            --> Just (Just 2)

    empty
        |> addValue (Maybe.map2 (+)) Scene 1
        |> get Scene
            --> Just Nothing

-}
addValue : (Maybe a -> Maybe b -> c) -> Aspect -> a -> Get b -> Get c
addValue composer =
    singleton >> (<<) (map2Value composer)


mapLists : (List a -> List a -> List a) -> Maybe (List a) -> Maybe (List a) -> List a
mapLists fu ma mb =
    fu (Maybe.withDefault [] ma) (Maybe.withDefault [] mb)


{-| Update the value associated with a specified key, if present

    import Ui.Layout.Aspect exposing (Aspect(..))

    singleton Scene "a"
        |> update Scene String.toUpper
        |> get Scene

    --> Just "A"

    singleton Scene "a"
        |> update Control String.toUpper
        |> get Scene

    --> Just "a"

    empty
        |> update Scene ((+) 1)
        |> get Scene

    --> Nothing

-}
update :
    Aspect
    -> (a -> a)
    -> Get a
    -> Get a
update key fu =
    mapByKey ((==) key >> Bool.ifElse fu identity)


{-| -}
updateValue :
    Aspect
    -> (Maybe a -> Maybe a)
    -> Get a
    -> Get a
updateValue key fu =
    mapValueByKey ((==) key >> Bool.ifElse fu identity)


{-|

    import Ui.Layout.Aspect exposing (Aspect(..))

    consList Scene 1 (full [2, 3])
        |> get Scene
        --> Just [1, 2, 3]

Implicitly creates a list if key has no value yet:

    consList Scene "a" empty
        |> get Scene
            --> Just ["a"]

-}
consList : Aspect -> a -> Get (List a) -> Get (List a)
consList =
    addValue
        (\ma mb ->
            Maybe.unwrap [] List.singleton ma
                ++ Maybe.withDefault [] mb
        )


{-| `addWithDefault [] (++)`

    import Ui.Layout.Aspect exposing (Aspect(..))

    addList Scene [1, 2] (full [3, 4])
        |> get Scene
        --> Just [1, 2, 3, 4]

    addList Scene [1, 2] (full [3, 4])
        |> get Control
        --> Just [3, 4]

-}
addList : Aspect -> List a -> Get (List a) -> Get (List a)
addList =
    addWithDefault [] (++)



-- MAP


{-| compose a function behind the result

    import Ui.Layout.Aspect exposing (Aspect(..))

    map negate (full 1)
        |> get Scene
        --> Just (-1)

-}
map : (a -> b) -> Get a -> Get b
map =
    Maybe.map >> (<<)


{-| Apply a sequence of operations on a `Get`:

    import Ui.Layout.Aspect exposing (Aspect(..))

    singleton Scene 1
        |> sequence
            [ map ((*) 2)
            , filter (modBy 2 >> (==) 0)
            , map ((+) 1)
            ]
        |> get Scene
        --> Just 3

-}
sequence : List (Get a -> Get a) -> Get a -> Get a
sequence operations getA =
    List.foldl
        (<|)
        getA
        operations


{-| only get a value if the condition holds

    import Ui.Layout.Aspect exposing (Aspect(..))

    singleton Scene 1
        |> insert Control 2
        |> insert Info 1
        |> filter ((==) 1)
        |> toList [Scene, Info, Control]
        --> [(Scene, 1), (Info, 1)]

-}
filter : (a -> Bool) -> Get a -> Get a
filter =
    Maybe.filter >> (<<)


{-| compose a function behind the result, preserving the 'Maybe' value.

    mapValue =
        (<<)

You can use this function to leverage mapping functions from the `Maybe` and `Maybe.Extra` library.
For example, `filter` can be expressed as `Maybe.filter >> mapValue`.

-}
mapValue : (Maybe a -> Maybe b) -> Get a -> Get b
mapValue =
    (<<)


{-| -}
mapByKey : (Aspect -> a -> b) -> Get a -> Get b
mapByKey fu getA key =
    Maybe.map (fu key) (getA key)


{-| -}
mapValueByKey : (Aspect -> Maybe a -> Maybe b) -> Get a -> Get b
mapValueByKey fu getA key =
    getA key |> fu key


{-| Compose a function behind the results of two `Get`s,
both with the identical key.
Note that if any of the two values is Nothing, the other is ignored.

Example:

    import Ui.Layout.Aspect exposing (Aspect(..))

    map2 (::) (singleton Scene 1) (singleton Scene [2, 3])
        |> get Scene
        --> Just [1, 2, 3]

-}
map2 : (a -> b -> c) -> Get a -> Get b -> Get c
map2 fu getA getB =
    \key -> Maybe.map2 fu (getA key) (getB key)


{-| -}
map2WithDefaults : a -> (a -> a -> b) -> Get a -> Get a -> Get b
map2WithDefaults default composer =
    map2Value (\m1 m2 -> composer (Maybe.withDefault default m1) (Maybe.withDefault default m2))


{-| Never fail
-}
map2Value : (Maybe a -> Maybe b -> c) -> Get a -> Get b -> Get c
map2Value fu getA getB =
    \key -> Just (fu (getA key) (getB key))


{-| -}
map2ByKey : (Aspect -> a -> b -> c) -> Get a -> Get b -> Get c
map2ByKey fu getA getB =
    \key -> Maybe.map2 (fu key) (getA key) (getB key)


{-| interprets Nothing as []

    import Ui.Layout.Aspect exposing (Aspect(..))

    singleton Scene [1]
        |> concat (singleton Scene [2, 3])
        |> get Scene
            --> Just [2, 3, 1]

    full [1]
        |> concat (singleton Scene [2, 3])
        |> get Control
            --> Just [1]

-}
concat : Get (List a) -> Get (List a) -> Get (List a)
concat =
    map2Value (mapLists (++))


{-| Get either the inner `Get`, else `empty`.
-}
flat : Get (Get a) -> Aspect -> Get a
flat =
    (<<) (Maybe.withDefault empty)


{-| `get` the inner `Get` with the given key

    import Ui.Layout.Aspect exposing (Aspect(..))

-}
unlockInner : Aspect -> Get (Get a) -> Get a
unlockInner key =
    map (get key) >> mapValue Maybe.join


{-| `get` the outer `Get` with the given key
-}
unlockOuter : Aspect -> Get (Get a) -> Get a
unlockOuter key =
    get key >> Maybe.withDefault empty


{-| Apply the same key twice
-}
join : Get (Get a) -> Get a
join get2 =
    \aspect -> flat get2 aspect aspect



-- COMPOSE


{-| returns the first parameter if the second fails, using the same key. Lazy.

    import Ui.Layout.Aspect exposing (Aspect(..))

    let
        getMy =
            empty
                |> orElse (singleton Info "b")
                |> orElse (singleton Scene "a")
                |> orElse (singleton Scene "c")
    in
    (getMy Scene, getMy Control)
    --> ( Just "a",  Nothing)

-}
orElse : Get a -> Get a -> Get a
orElse second first key =
    first key
        |> Maybe.orElseLazy (\() -> second key)


{-| -}
andGet : Aspect -> Maybe (Get a) -> Maybe a
andGet =
    get >> Maybe.andThen


{-| get `get b` from the result of `get a`

    import Ui.Layout.Aspect exposing (Aspect(..))

    full 2
        |> andThen (negate >> singleton Scene)
        |> get Control
        |> andGet Scene
        --> Just (-2)

-}
andThen : (a -> Get b) -> Get a -> Get (Get b)
andThen createGetB getA =
    getA
        >> Maybe.unwrap
            empty
            createGetB
        >> Just


{-| Applies the same key twice to unlock a nested `Get`

`andThenFlat = andThen >> (<<) join`

    import Ui.Layout.Aspect exposing (Aspect(..))

    full 2
        |> andThenFlat (negate >> singleton Scene)
        |> get Scene
        --> Just (-2)

-}
andThenFlat : (a -> Get b) -> Get a -> Get b
andThenFlat =
    andThen >> (<<) join


{-| Like `andThen`, but you supply a fallible creator function
-}
andThenMaybe : (a -> Maybe (Get b)) -> Get a -> Get (Get b)
andThenMaybe =
    Maybe.andThen
        >> (<<)


{-| Map the result of the first `get` to the next `get`
-}
andMap : Get (a -> b) -> Get a -> Get (Get b)
andMap getAB getA =
    getAB
        >> Maybe.map
            (\ab ->
                map ab getA
            )


{-| like `andMap`, but apply the same key twice
-}
andMapFlat : Get (a -> b) -> Get a -> Get b
andMapFlat =
    andMap >> (<<) join



-- QUERY


{-| Example:

    import Ui.Layout.Aspect exposing (Aspect(..))

    singleton Scene 1
        |> member Scene --> True

    singleton Control 1
        |> member Scene -> False

-}
member : Aspect -> Get a -> Bool
member =
    get >> (<<) Maybe.isJust


{-| `(|>)`
-}
get : key -> (key -> value) -> value
get =
    (|>)


{-| Note that, strictly speaking, you no longer have a `Get` after applying `withDefault`.

    import Ui.Layout.Aspect exposing (Aspect(..))

    empty
        |> withDefault "default"
        |> get Scene
        --> "default"

-}
withDefault : a -> Get a -> (Aspect -> a)
withDefault =
    Maybe.withDefault
        >> (<<)



-- LISTS


{-|

    import Ui.Layout.Aspect exposing (Aspect(..))

    full "Hello"
        |> toList [Scene, Control]
        --> [(Scene, "Hello"), (Control, "Hello")]

-}
toList : List Aspect -> Get a -> List ( Aspect, a )
toList list getA =
    List.foldr (\key -> getA key |> Maybe.map (Tuple.pair key) |> Maybe.cons) [] list


{-| `orAdd`s from the list but skips duplicate keys.
`fromList = fromListWith orAdd`

If you want to modify the behavior on duplicate keys, use `fromListWith`
or `consList`.

    import Ui.Layout.Aspect exposing (Aspect(..))

    fromList [(Scene, "Scene 1"), (Scene, "Scene 2"), (Control, "C")]
        |> toList [Scene, Control]
        --> [(Scene, "Scene 1"), (Control, "C")]

-}
fromList : List ( Aspect, a ) -> Get a
fromList =
    --fromListWith : (Aspect -> a -> Get b -> Get b) -> List ( Aspect, a ) -> Get b
    fromListBy orAdd


{-| -}
toListBy : Get (a -> b) -> List Aspect -> Get a -> List b
toListBy getAB aspects =
    andMap getAB >> join >> values aspects


{-|

    import Ui.Layout.Aspect exposing (Aspect(..))

    values [Scene, Control] (full 2) --> [2, 2]

-}
values : List Aspect -> Get a -> List a
values list getA =
    List.foldr (getA >> Maybe.cons) [] list


{-| -}
keys : List Aspect -> Get a -> List Aspect
keys list getA =
    List.foldr (\key -> getA key |> Maybe.unwrap identity (\_ -> (::) key)) [] list


{-| `concatLists = fromListBy addList`
-}
concatLists : List ( Aspect, List a ) -> Get (List a)
concatLists =
    fromListBy addList


{-| `consLists = fromListBy consList`
-}
consLists : List ( Aspect, a ) -> Get (List a)
consLists =
    fromListBy consList


{-| -}
fromListBy : (Aspect -> a -> Get b -> Get b) -> List ( Aspect, a ) -> Get b
fromListBy fu =
    List.map
        (\( key, a ) -> fu key a)
        >> (\operations -> sequence operations empty)
