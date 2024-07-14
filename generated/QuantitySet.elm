module QuantitySet exposing
    ( empty, singleton, insert, remove
    , toList, fromList
    , isEmpty, member, size
    , QuantitySet
    , foldl, foldr, filter, partition
    )

{-|


## Build

@docs empty, singleton, insert, remove


## Lists

@docs toList, fromList


## Query

@docs isEmpty, member, size


## Sets

@docs QuantitySet


## Transform

@docs foldl, foldr, filter, partition

-}

import FastDict
import Quantity


type QuantitySet number units
    = QuantitySet (FastDict.Dict number (Quantity.Quantity number units))


empty : QuantitySet number units
empty =
    QuantitySet FastDict.empty


singleton : Quantity.Quantity number units -> QuantitySet number units
singleton value =
    QuantitySet (FastDict.singleton (Quantity.unwrap value) value)


insert : Quantity.Quantity number units -> QuantitySet number units -> QuantitySet number units
insert value d =
    case d of
        QuantitySet dict ->
            QuantitySet (FastDict.insert (Quantity.unwrap value) value dict)


remove : Quantity.Quantity number units -> QuantitySet number units -> QuantitySet number units
remove value d =
    case d of
        QuantitySet dict ->
            QuantitySet (FastDict.remove (Quantity.unwrap value) dict)


isEmpty : QuantitySet number units -> Bool
isEmpty d =
    case d of
        QuantitySet dict ->
            FastDict.isEmpty dict


member : Quantity.Quantity number units -> QuantitySet number units -> Bool
member value d =
    case d of
        QuantitySet dict ->
            FastDict.member (Quantity.unwrap value) dict


size : QuantitySet number units -> Int
size d =
    case d of
        QuantitySet dict ->
            FastDict.size dict


toList : QuantitySet number units -> List (Quantity.Quantity number units)
toList d =
    case d of
        QuantitySet dict ->
            FastDict.values dict


fromList : List (Quantity.Quantity number units) -> QuantitySet number units
fromList l =
    QuantitySet (FastDict.fromList (List.map (\e -> ( Quantity.unwrap e, e )) l))


foldl : (Quantity.Quantity number units -> b -> b) -> b -> QuantitySet number units -> b
foldl f b0 d =
    case d of
        QuantitySet dict ->
            FastDict.foldl (\_ e b -> f e b) b0 dict


foldr : (Quantity.Quantity number units -> b -> b) -> b -> QuantitySet number units -> b
foldr f b0 d =
    case d of
        QuantitySet dict ->
            FastDict.foldr (\_ e b -> f e b) b0 dict


filter : (Quantity.Quantity number units -> Bool) -> QuantitySet number units -> QuantitySet number units
filter f d =
    QuantitySet
        (case d of
            QuantitySet dict ->
                FastDict.filter (\filterUnpack -> f) dict
        )


partition : (Quantity.Quantity number units -> Bool) -> QuantitySet number units -> ( QuantitySet number units, QuantitySet number units )
partition f d =
    case d of
        QuantitySet dict ->
            Tuple.mapBoth
                QuantitySet
                QuantitySet
                (FastDict.partition (\partitionUnpack -> f) dict)
