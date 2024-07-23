module IdSet exposing
    ( empty, singleton, insert, remove
    , toList, fromList
    , isEmpty, member, size
    , IdSet
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

@docs IdSet


## Transform

@docs foldl, foldr, filter, partition

-}

import FastDict
import Id


type IdSet kind
    = IdSet (FastDict.Dict String (Id.Id kind))


empty : IdSet kind
empty =
    IdSet FastDict.empty


singleton : Id.Id kind -> IdSet kind
singleton value =
    IdSet (FastDict.singleton (Id.toString value) value)


insert : Id.Id kind -> IdSet kind -> IdSet kind
insert value d =
    case d of
        IdSet dict ->
            IdSet (FastDict.insert (Id.toString value) value dict)


remove : Id.Id kind -> IdSet kind -> IdSet kind
remove value d =
    case d of
        IdSet dict ->
            IdSet (FastDict.remove (Id.toString value) dict)


isEmpty : IdSet kind -> Bool
isEmpty d =
    case d of
        IdSet dict ->
            FastDict.isEmpty dict


member : Id.Id kind -> IdSet kind -> Bool
member value d =
    case d of
        IdSet dict ->
            FastDict.member (Id.toString value) dict


size : IdSet kind -> Int
size d =
    case d of
        IdSet dict ->
            FastDict.size dict


toList : IdSet kind -> List (Id.Id kind)
toList d =
    case d of
        IdSet dict ->
            FastDict.values dict


fromList : List (Id.Id kind) -> IdSet kind
fromList l =
    IdSet (FastDict.fromList (List.map (\e -> ( Id.toString e, e )) l))


foldl : (Id.Id kind -> b -> b) -> b -> IdSet kind -> b
foldl f b0 d =
    case d of
        IdSet dict ->
            FastDict.foldl (\_ e b -> f e b) b0 dict


foldr : (Id.Id kind -> b -> b) -> b -> IdSet kind -> b
foldr f b0 d =
    case d of
        IdSet dict ->
            FastDict.foldr (\_ e b -> f e b) b0 dict


filter : (Id.Id kind -> Bool) -> IdSet kind -> IdSet kind
filter f d =
    IdSet
        (case d of
            IdSet dict ->
                FastDict.filter (\filterUnpack -> f) dict
        )


partition : (Id.Id kind -> Bool) -> IdSet kind -> ( IdSet kind, IdSet kind )
partition f d =
    case d of
        IdSet dict ->
            Tuple.mapBoth
                IdSet
                IdSet
                (FastDict.partition (\partitionUnpack -> f) dict)
