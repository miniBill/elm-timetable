module IdDict exposing (IdDict, empty, filter, foldl, foldr, fromList, get, getMax, getMaxKey, getMin, getMinKey, insert, isEmpty, keys, map, member, partition, popMax, popMin, remove, singleton, size, toList, update, values)

{-| 
## Build

@docs empty, singleton, insert, update, remove

## Dictionaries

@docs IdDict

## Lists

@docs keys, values, toList, fromList

## Min / Max

@docs getMinKey, getMin, popMin, getMaxKey, getMax, popMax

## Query

@docs isEmpty, member, get, size

## Transform

@docs map, foldl, foldr, filter, partition
-}


import FastDict
import Id


type IdDict kind v
    = IdDict (FastDict.Dict String ( Id.Id kind, v ))


empty : IdDict kind v
empty =
    IdDict FastDict.empty


singleton : Id.Id kind -> v -> IdDict kind v
singleton key value =
    IdDict (FastDict.singleton (Id.toString key) ( key, value ))


insert : Id.Id kind -> v -> IdDict kind v -> IdDict kind v
insert key value d =
    case d of
        IdDict dict ->
            IdDict (FastDict.insert (Id.toString key) ( key, value ) dict)


update : Id.Id kind -> (Maybe b -> Maybe b) -> IdDict kind b -> IdDict kind b
update key f d =
    case d of
        IdDict dict ->
            IdDict
                (FastDict.update
                     (Id.toString key)
                     (\updateUnpack ->
                          Maybe.map
                              (Tuple.pair key)
                              (f (Maybe.map Tuple.second updateUnpack))
                     )
                     dict
                )


remove : Id.Id kind -> IdDict kind v -> IdDict kind v
remove key d =
    case d of
        IdDict dict ->
            IdDict (FastDict.remove (Id.toString key) dict)


isEmpty : IdDict kind v -> Bool
isEmpty d =
    case d of
        IdDict dict ->
            FastDict.isEmpty dict


member : Id.Id kind -> IdDict kind v -> Bool
member key d =
    case d of
        IdDict dict ->
            FastDict.member (Id.toString key) dict


get : Id.Id kind -> IdDict kind b -> Maybe b
get key d =
    case d of
        IdDict dict ->
            Maybe.map Tuple.second (FastDict.get (Id.toString key) dict)


size : IdDict kind v -> Int
size d =
    case d of
        IdDict dict ->
            FastDict.size dict


keys : IdDict kind v -> List (Id.Id kind)
keys d =
    case d of
        IdDict dict ->
            List.map Tuple.first (FastDict.values dict)


values : IdDict kind v -> List v
values d =
    case d of
        IdDict dict ->
            List.map Tuple.second (FastDict.values dict)


toList : IdDict kind v -> List ( Id.Id kind, v )
toList d =
    case d of
        IdDict dict ->
            FastDict.values dict


fromList : List ( Id.Id kind, v ) -> IdDict kind v
fromList l =
    IdDict
        (FastDict.fromList
             (List.map
                  (\e ->
                       case e of
                           ( k, v ) ->
                               ( Id.toString k, e )
                  )
                  l
             )
        )


map : (Id.Id kind -> a -> b) -> IdDict kind a -> IdDict kind b
map f d =
    case d of
        IdDict dict ->
            IdDict
                (FastDict.map
                     (\mapUnpack ->
                          \unpack ->
                              case unpack of
                                  ( k, a ) ->
                                      ( k, f k a )
                     )
                     dict
                )


foldl : (Id.Id kind -> v -> b -> b) -> b -> IdDict kind v -> b
foldl f b0 d =
    case d of
        IdDict dict ->
            FastDict.foldl
                (\_ kv b ->
                     case kv of
                         ( k, v ) ->
                             f k v b
                )
                b0
                dict


foldr : (Id.Id kind -> v -> b -> b) -> b -> IdDict kind v -> b
foldr f b0 d =
    case d of
        IdDict dict ->
            FastDict.foldr
                (\_ kv b ->
                     case kv of
                         ( k, v ) ->
                             f k v b
                )
                b0
                dict


filter : (Id.Id kind -> v -> Bool) -> IdDict kind v -> IdDict kind v
filter f d =
    IdDict
        (case d of
             IdDict dict ->
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
    (Id.Id kind -> v -> Bool)
    -> IdDict kind v
    -> ( IdDict kind v, IdDict kind v )
partition f d =
    case d of
        IdDict dict ->
            Tuple.mapBoth
                IdDict
                IdDict
                (FastDict.partition
                     (\partitionUnpack ->
                          \unpack ->
                              case unpack of
                                  ( k, v ) ->
                                      f k v
                     )
                     dict
                )


getMinKey : IdDict kind v -> Maybe String
getMinKey d =
    case d of
        IdDict dict ->
            FastDict.getMinKey dict


getMin : IdDict kind v -> Maybe ( String, ( Id.Id kind, v ) )
getMin d =
    case d of
        IdDict dict ->
            FastDict.getMin dict


popMin :
    IdDict kind v
    -> Maybe ( ( String, ( Id.Id kind, v ) ), FastDict.Dict String ( Id.Id kind, v ) )
popMin d =
    case d of
        IdDict dict ->
            FastDict.popMin dict


getMaxKey : IdDict kind v -> Maybe String
getMaxKey d =
    case d of
        IdDict dict ->
            FastDict.getMaxKey dict


getMax : IdDict kind v -> Maybe ( String, ( Id.Id kind, v ) )
getMax d =
    case d of
        IdDict dict ->
            FastDict.getMax dict


popMax :
    IdDict kind v
    -> Maybe ( ( String, ( Id.Id kind, v ) ), FastDict.Dict String ( Id.Id kind, v ) )
popMax d =
    case d of
        IdDict dict ->
            FastDict.popMax dict