module QuantityDict exposing
    ( empty, singleton, insert, update, remove
    , QuantityDict
    , keys, values, toList, fromList
    , getMinKey, getMaxKey
    , isEmpty, member, get, size
    , map, foldl, foldr, filter, partition
    )

{-|


## Build

@docs empty, singleton, insert, update, remove


## Dictionaries

@docs QuantityDict


## Lists

@docs keys, values, toList, fromList


## Min / Max

@docs getMinKey, getMaxKey


## Query

@docs isEmpty, member, get, size


## Transform

@docs map, foldl, foldr, filter, partition

-}

import FastDict
import Quantity


type QuantityDict number units v
    = QuantityDict (FastDict.Dict number ( Quantity.Quantity number units, v ))


empty : QuantityDict number units v
empty =
    QuantityDict FastDict.empty


singleton : Quantity.Quantity number units -> v -> QuantityDict number units v
singleton key value =
    QuantityDict (FastDict.singleton (Quantity.unwrap key) ( key, value ))


insert : Quantity.Quantity number units -> v -> QuantityDict number units v -> QuantityDict number units v
insert key value d =
    case d of
        QuantityDict dict ->
            QuantityDict (FastDict.insert (Quantity.unwrap key) ( key, value ) dict)


update : Quantity.Quantity number units -> (Maybe b -> Maybe b) -> QuantityDict number units b -> QuantityDict number units b
update key f d =
    case d of
        QuantityDict dict ->
            QuantityDict
                (FastDict.update
                    (Quantity.unwrap key)
                    (\updateUnpack ->
                        Maybe.map
                            (Tuple.pair key)
                            (f (Maybe.map Tuple.second updateUnpack))
                    )
                    dict
                )


remove : Quantity.Quantity number units -> QuantityDict number units v -> QuantityDict number units v
remove key d =
    case d of
        QuantityDict dict ->
            QuantityDict (FastDict.remove (Quantity.unwrap key) dict)


isEmpty : QuantityDict number units v -> Bool
isEmpty d =
    case d of
        QuantityDict dict ->
            FastDict.isEmpty dict


member : Quantity.Quantity number units -> QuantityDict number units v -> Bool
member key d =
    case d of
        QuantityDict dict ->
            FastDict.member (Quantity.unwrap key) dict


get : Quantity.Quantity number units -> QuantityDict number units b -> Maybe b
get key d =
    case d of
        QuantityDict dict ->
            Maybe.map Tuple.second (FastDict.get (Quantity.unwrap key) dict)


size : QuantityDict number units v -> Int
size d =
    case d of
        QuantityDict dict ->
            FastDict.size dict


keys : QuantityDict number units v -> List (Quantity.Quantity number units)
keys d =
    case d of
        QuantityDict dict ->
            List.map Tuple.first (FastDict.values dict)


values : QuantityDict number units v -> List v
values d =
    case d of
        QuantityDict dict ->
            List.map Tuple.second (FastDict.values dict)


toList : QuantityDict number units v -> List ( Quantity.Quantity number units, v )
toList d =
    case d of
        QuantityDict dict ->
            FastDict.values dict


fromList : List ( Quantity.Quantity number units, v ) -> QuantityDict number units v
fromList l =
    QuantityDict
        (FastDict.fromList
            (List.map
                (\e ->
                    case e of
                        ( k, v ) ->
                            ( Quantity.unwrap k, e )
                )
                l
            )
        )


map : (Quantity.Quantity number units -> a -> b) -> QuantityDict number units a -> QuantityDict number units b
map f d =
    case d of
        QuantityDict dict ->
            QuantityDict
                (FastDict.map
                    (\mapUnpack ->
                        \unpack ->
                            case unpack of
                                ( k, a ) ->
                                    ( k, f k a )
                    )
                    dict
                )


foldl : (Quantity.Quantity number units -> v -> b -> b) -> b -> QuantityDict number units v -> b
foldl f b0 d =
    case d of
        QuantityDict dict ->
            FastDict.foldl
                (\_ kv b ->
                    case kv of
                        ( k, v ) ->
                            f k v b
                )
                b0
                dict


foldr : (Quantity.Quantity number units -> v -> b -> b) -> b -> QuantityDict number units v -> b
foldr f b0 d =
    case d of
        QuantityDict dict ->
            FastDict.foldr
                (\_ kv b ->
                    case kv of
                        ( k, v ) ->
                            f k v b
                )
                b0
                dict


filter : (Quantity.Quantity number units -> v -> Bool) -> QuantityDict number units v -> QuantityDict number units v
filter f d =
    QuantityDict
        (case d of
            QuantityDict dict ->
                FastDict.filter
                    (\filterUnpack ->
                        \unpack ->
                            case unpack of
                                ( k, v ) ->
                                    f k v
                    )
                    dict
        )


partition :
    (Quantity.Quantity number units -> v -> Bool)
    -> QuantityDict number units v
    -> ( QuantityDict number units v, QuantityDict number units v )
partition f d =
    case d of
        QuantityDict dict ->
            Tuple.mapBoth
                QuantityDict
                QuantityDict
                (FastDict.partition
                    (\partitionUnpack ->
                        \unpack ->
                            case unpack of
                                ( k, v ) ->
                                    f k v
                    )
                    dict
                )


getMinKey : QuantityDict number units v -> Maybe number
getMinKey d =
    case d of
        QuantityDict dict ->
            FastDict.getMinKey dict


getMaxKey : QuantityDict number units v -> Maybe number
getMaxKey d =
    case d of
        QuantityDict dict ->
            FastDict.getMaxKey dict
