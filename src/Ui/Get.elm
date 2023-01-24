module Ui.Get exposing
    ( Get
    , empty, full, singleton
    , orAdd, insert, update, updateValue, updateWhere
    , addValue, addWithDefault, maybeAdd
    , remove
    , consList, addList
    , map, mapValue, map2, map2WithDefaults, map2Value, filter
    , mapKey
    , mapByKey, mapValueByKey, map2ByKey
    , append
    , join, unlockInner, unlockOuter
    , orElse
    , andThen, andThenFlat, andThenMaybe, andMap, andMapFlat
    , union, intersect, diff
    , member, andGet, withDefault
    , toList, toListBy, keys, values, fromList, fromListBy
    , consLists, concatLists
    , sequence
    , get
    )

{-| Dictionary [`Aspect`](Ui.Layout.Aspect) -> `ui`

@docs Get


# Create

@docs empty, full, singleton


# Associate values

@docs orAdd, insert, update, updateValue, updateWhere
@docs addValue, addWithDefault, maybeAdd
@docs remove


### Add to `Get (List a)`

@docs consList, addList


# Modify

@docs map, mapValue, map2, map2WithDefaults, map2Value, filter


### Map the key parameter

@docs mapKey


### Map with respect to the key

@docs mapByKey, mapValueByKey, map2ByKey


### Map `Get (List a)`

@docs append


### Map `Get (Get a)`

@docs join, unlockInner, unlockOuter


# Compose


### Or

@docs orElse


### Chain

@docs andThen, andThenFlat, andThenMaybe, andMap, andMapFlat


### Set operations

@docs union, intersect, diff


# Query

@docs member, andGet, withDefault


# Lists

@docs toList, toListBy, keys, values, fromList, fromListBy


### Convenience functions for `Get (List a)`

@docs consLists, concatLists


### Folding

@docs sequence


# Apply

@docs get

-}

import Bool.Extra as Bool
import Maybe.Extra as Maybe


{-| -}
type alias Get key a =
    key -> Maybe a



-- CREATE


{-| in any case, return Nothing
-}
empty : Get key a
empty =
    always Nothing


{-| in any case, return `a`

    import Ui.Layout.Aspect exposing (Aspect(..))

    full 1
        |> get Scene
        --> Just 1

-}
full : a -> Get key a
full =
    Just >> always


{-| -}
singleton : key -> a -> Get key a
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
orAdd : key -> a -> Get key a -> Get key a
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
insert : key -> a -> Get key a -> Get key a
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
maybeAdd : (a -> b -> c) -> key -> a -> Get key b -> Get key c
maybeAdd =
    map2 >> composeWithTwoParameters singleton


{-| Define a default so that the composition can't fail

    import Ui.Layout.Aspect exposing (Aspect(..))

    addWithDefault 0 (+) Scene 1 (full 2)
        |> get Scene
        --> Just 3

    addWithDefault 0 (+) Scene 1 empty
        |> get Scene
        --> Just 1

-}
addWithDefault : a -> (a -> a -> b) -> key -> a -> Get key a -> Get key b
addWithDefault default =
    map2WithDefaults default
        >> composeWithTwoParameters singleton


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
addValue : (Maybe a -> Maybe b -> c) -> key -> a -> Get key b -> Get key c
addValue =
    map2Value
        >> composeWithTwoParameters singleton


{-| -}
remove : key -> Get key a -> Get key a
remove a get_ aspect =
    if aspect == a then
        Nothing

    else
        get_ aspect


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
    key
    -> (a -> a)
    -> Get key a
    -> Get key a
update key fu =
    mapByKey ((==) key >> Bool.ifElse fu identity)


{-| -}
updateValue :
    key
    -> (Maybe a -> Maybe a)
    -> Get key a
    -> Get key a
updateValue key fu =
    mapValueByKey ((==) key >> Bool.ifElse fu identity)


{-|

    import Ui.Layout.Aspect exposing (Aspect(..))

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
updateWhere condition fu getA key =
    Maybe.map
        (if condition key then
            fu

         else
            identity
        )
        (getA key)


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
consList : key -> a -> Get key (List a) -> Get key (List a)
consList =
    addValue
        (\ma mb ->
            Maybe.unwrap [] List.singleton ma
                ++ Maybe.withDefault [] mb
        )


{-| `addWithDefault [] (++)`

    import Ui.Layout.Aspect exposing (Aspect(..))

    full [1, 2]
        |> addList Scene [3, 4]
        |> get Scene
        --> Just [1, 2, 3, 4]

    full [1, 2]
        |> addList Scene [3, 4]
        |> get Control
        --> Just [1, 2]

-}
addList : key -> List a -> Get key (List a) -> Get key (List a)
addList =
    addWithDefault [] (\x y -> y ++ x)



-- MAP


{-| compose a function behind the result

    import Ui.Layout.Aspect exposing (Aspect(..))

    map negate (full 1)
        |> get Scene
        --> Just (-1)

-}
map : (a -> b) -> Get key a -> Get key b
map =
    mapParameter Maybe.map


{-|

    singleton 1 ()
        |> mapKey identity
        |> get (Just 1)
        --> Just ()

    singleton "1" ()
        |> mapKey String.fromInt
        |> get 1
        --> Just ()

-}
mapKey : (key2 -> Maybe key1) -> Get key1 a -> Get key2 a
mapKey discard get1 =
    discard >> Maybe.andThen get1


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
sequence : List (Get key a -> Get key a) -> Get key a -> Get key a
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
filter : (a -> Bool) -> Get key a -> Get key a
filter =
    mapParameter Maybe.filter


{-| compose a function behind the result, preserving the 'Maybe' value.

`mapValue = (<<)`

You can use this function to leverage mapping functions from the `Maybe` and `Maybe.Extra` library.

    import Maybe
    import Ui.Layout.Aspect exposing (Aspect(..))

    withDefault : a -> Maybe a -> Maybe a
    withDefault a =
        Maybe.withDefault a >> Just

    fromList [ (Scene, 1), (Control, 2)]
        |> mapValue (withDefault 4)
        |> toList [Scene, Control, Info]
        --> [ (Scene, 1), (Control, 2), (Info, 4) ]

-}
mapValue : (Maybe a -> Maybe b) -> Get key a -> Get key b
mapValue =
    (<<)


{-| Parametrize map over key.
Same as andMapFlat!
-}
mapByKey : (key -> a -> b) -> Get key a -> Get key b
mapByKey fu getA key =
    Maybe.map (fu key) (getA key)


{-| -}
mapValueByKey : (key -> Maybe a -> Maybe b) -> Get key a -> Get key b
mapValueByKey fu getA key =
    getA key
        |> fu key


{-| Compose a function behind the results of two `Get`s,
both with the identical key.
Note that if any of the two values is Nothing, the other is ignored.

Example:

    import Ui.Layout.Aspect exposing (Aspect(..))

    map2 (::) (singleton Scene 1) (singleton Scene [2, 3])
        |> get Scene
        --> Just [1, 2, 3]

-}
map2 : (a -> b -> c) -> Get key a -> Get key b -> Get key c
map2 fu getA getB =
    \key -> Maybe.map2 fu (getA key) (getB key)


{-| -}
map2WithDefaults : a -> (a -> a -> b) -> Get key a -> Get key a -> Get key b
map2WithDefaults default composer =
    map2Value (\m1 m2 -> composer (Maybe.withDefault default m1) (Maybe.withDefault default m2))


{-| Never fail
-}
map2Value : (Maybe a -> Maybe b -> c) -> Get key a -> Get key b -> Get key c
map2Value fu getA getB =
    \key ->
        getB key
            |> fu (getA key)
            |> Just


{-| `map2Maybe fu getA getB = \key -> fu (getA key) (getB key)`
-}
map2Maybe : (Maybe a -> Maybe b -> Maybe c) -> Get key a -> Get key b -> Get key c
map2Maybe fu getA getB =
    \key -> fu (getA key) (getB key)


{-| -}
map2ByKey : (key -> a -> b -> c) -> Get key a -> Get key b -> Get key c
map2ByKey fu getA getB =
    \key -> Maybe.map2 (fu key) (getA key) (getB key)


{-| Interprets Nothing as [].

Difference to `addList`: Both lists to append can be Nothing.

    import Ui.Layout.Aspect exposing (Aspect(..))

    singleton Scene [1]
        |> append (singleton Scene [2, 3])
        |> get Scene
            --> Just [2, 3, 1]

    full [1]
        |> append (singleton Scene [2, 3])
        |> get Control
            --> Just [1]

-}
append : Get key (List a) -> Get key (List a) -> Get key (List a)
append =
    map2Value (mapLists (++))


{-| Get either the inner `Get`, else `empty`.
-}
flat : Get key (Get key a) -> key -> Get key a
flat g =
    g >> Maybe.withDefault empty


{-| `get` the inner `Get` with the given key

    import Ui.Layout.Aspect exposing (Aspect(..))

-}
unlockInner : key -> Get key (Get key a) -> Get key a
unlockInner key =
    map (get key) >> mapValue Maybe.join


{-| `get` the outer `Get` with the given key
-}
unlockOuter : key -> Get key (Get key a) -> Get key a
unlockOuter key =
    get key >> Maybe.withDefault empty


{-| Apply the same key twice
-}
join : Get key (Get key a) -> Get key a
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
orElse : Get key a -> Get key a -> Get key a
orElse second first key =
    first key
        |> Maybe.orElseLazy (\() -> second key)



---- Set operations ----


{-| -}
union : (a -> a -> a) -> Get key a -> Get key a -> Get key a
union handleCollision =
    map2Maybe <|
        \maybeA maybeB ->
            case ( maybeA, maybeB ) of
                ( Just a, Just b ) ->
                    Just (handleCollision a b)

                ( Just a, Nothing ) ->
                    Just a

                ( Nothing, Just b ) ->
                    Just b

                ( Nothing, Nothing ) ->
                    Nothing


{-| -}
intersect : Get key a -> Get key b -> Get key ( a, b )
intersect =
    Maybe.map2 Tuple.pair
        |> map2Maybe


{-| `diff a b` is b - a in the sense that we "pair a to the set of b"
-}
diff : Get key a -> Get key b -> Get key ( Maybe a, b )
diff =
    map2Maybe <|
        \maybeB ->
            Maybe.andThen (Tuple.pair maybeB >> Just)



----


{-| -}
andGet : key -> Maybe (Get key a) -> Maybe a
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
andThen : (a -> Get key b) -> Get key a -> Get key (Get key b)
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
andThenFlat : (a -> Get key b) -> Get key a -> Get key b
andThenFlat =
    composeWithTwoParameters andThen join


{-| Like `andThen`, but you supply a fallible creator function
-}
andThenMaybe : (a -> Maybe (Get key b)) -> Get key a -> Get key (Get key b)
andThenMaybe =
    mapParameter Maybe.andThen


{-| Map the result of the first `get` to the next `get`
-}
andMap : Get key (a -> b) -> Get key a -> Get key (Get key b)
andMap getAB getA =
    getAB
        >> Maybe.map
            (\ab ->
                map ab getA
            )


{-| Like `andMap`, but apply the same key twice.
Same as `mapByKey`!
-}
andMapFlat : Get key (a -> b) -> Get key a -> Get key b
andMapFlat =
    composeWithTwoParameters andMap join



-- QUERY


{-| Example:

    import Ui.Layout.Aspect exposing (Aspect(..))

    singleton Scene 1
        |> member Scene --> True

    singleton Control 1
        |> member Scene -> False

-}
member : key -> Get key a -> Bool
member =
    composeWithTwoParameters get Maybe.isJust



-- Maybe a -> Bool                                       /


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
withDefault : a -> Get key a -> (key -> a)
withDefault =
    mapParameter Maybe.withDefault



-- LISTS


{-|

    import Ui.Layout.Aspect exposing (Aspect(..))

    full "Hello"
        |> toList [Scene, Control]
        --> [(Scene, "Hello"), (Control, "Hello")]

-}
toList : List key -> Get key a -> List ( key, a )
toList list getA =
    List.foldr
        (\key ->
            getA key
                |> Maybe.map (Tuple.pair key)
                |> Maybe.cons
        )
        []
        list


{-| `orAdd`s from the list but skips duplicate keys.
`fromList = fromListWith orAdd`

If you want to modify the behavior on duplicate keys, use `fromListWith`
or `consList`.

    import Ui.Layout.Aspect exposing (Aspect(..))

    fromList [(Scene, "Scene 1"), (Scene, "Scene 2"), (Control, "C")]
        |> toList [Scene, Control]
        --> [(Scene, "Scene 1"), (Control, "C")]

-}
fromList : List ( key, a ) -> Get key a
fromList =
    --fromListWith : (Aspect -> a -> Get b -> Get b) -> List ( Aspect, a ) -> Get b
    fromListBy orAdd


{-| -}
toListBy : Get key (a -> b) -> List key -> Get key a -> List b
toListBy getAB aspects =
    andMapFlat getAB >> values aspects


{-|

    import Ui.Layout.Aspect exposing (Aspect(..))

    values [Scene, Control] (full 2) --> [2, 2]

-}
values : List key -> Get key a -> List a
values list getA =
    List.foldr (getA >> Maybe.cons) [] list


{-| -}
keys : List key -> Get key a -> List key
keys list getA =
    List.foldr
        (\key ->
            getA key
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



---- Helper ----


{-| This is a strangely important function that deserves an important name.

`mapParameter a = a >> (<<)`
`mapParameter a b = (|>) >> (a b)`
`mapParameter a b c = c >> (a b)`
`mapParameter a b c d = (a b (c d))`

-}
mapParameter :
    (a
     -> (b -> c)
    )
    ->
        (a
         -> ((input -> b) -> (input -> c))
        )
mapParameter function =
    let
        composeRightToLeft : (b -> c) -> (input -> b) -> (input -> c)
        composeRightToLeft =
            (<<)
    in
    function >> composeRightToLeft


composeWithTwoParameters : (a -> b -> c) -> (c -> d) -> a -> b -> d
composeWithTwoParameters function0 function1 a =
    -- compose left-to-right with two parameters
    function0 a >> function1
