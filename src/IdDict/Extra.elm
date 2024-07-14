module IdDict.Extra exposing (groupBy, merge, union)

import Id exposing (Id)
import IdDict exposing (IdDict)


groupBy :
    (a -> Id kind)
    -> List a
    -> IdDict kind (List a)
groupBy keyfn list =
    List.foldr
        (\x acc ->
            IdDict.update (keyfn x) (Maybe.map ((::) x) >> Maybe.withDefault [ x ] >> Just) acc
        )
        IdDict.empty
        list


merge :
    (Id kind -> lv -> a -> a)
    -> (Id kind -> lv -> rv -> a -> a)
    -> (Id kind -> rv -> a -> a)
    -> IdDict kind lv
    -> IdDict kind rv
    -> a
    -> a
merge onLeft onBoth onRight left right acc =
    let
        go : List ( Id kind, lv ) -> List ( Id kind, rv ) -> a -> a
        go lqueue rqueue iacc =
            case lqueue of
                [] ->
                    case rqueue of
                        [] ->
                            iacc

                        ( rheadKey, rheadValue ) :: rtail ->
                            go lqueue rtail (onRight rheadKey rheadValue iacc)

                ( lheadKey, lheadValue ) :: ltail ->
                    case rqueue of
                        [] ->
                            go ltail rqueue (onLeft lheadKey lheadValue iacc)

                        ( rheadKey, rheadValue ) :: rtail ->
                            case Id.compare lheadKey rheadKey of
                                EQ ->
                                    go ltail rtail (onBoth lheadKey lheadValue rheadValue iacc)

                                LT ->
                                    go ltail rqueue (onLeft lheadKey lheadValue iacc)

                                GT ->
                                    go lqueue rtail (onRight rheadKey rheadValue iacc)
    in
    go (IdDict.toList left) (IdDict.toList right) acc


union : IdDict kind value -> IdDict kind value -> IdDict kind value
union l r =
    let
        _ =
            Debug.todo
    in
    IdDict.fromList (IdDict.toList l ++ IdDict.toList r)
