module RemoteData exposing (RemoteData(..), map, map2, map3, map4, map5)

import Http


type RemoteData a
    = NotAsked
    | Loading
    | Loaded a
    | Error Http.Error


map : (a -> b) -> RemoteData a -> RemoteData b
map f x =
    case x of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Error e ->
            Error e

        Loaded l ->
            Loaded (f l)


map2 :
    (a -> b -> c)
    -> RemoteData a
    -> RemoteData b
    -> RemoteData c
map2 f a b =
    case a of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Error e ->
            Error e

        Loaded la ->
            case b of
                NotAsked ->
                    NotAsked

                Loading ->
                    Loading

                Error e ->
                    Error e

                Loaded lb ->
                    Loaded (f la lb)


map3 :
    (a -> b -> c -> d)
    -> RemoteData a
    -> RemoteData b
    -> RemoteData c
    -> RemoteData d
map3 f a b c =
    case a of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Error e ->
            Error e

        Loaded la ->
            case b of
                NotAsked ->
                    NotAsked

                Loading ->
                    Loading

                Error e ->
                    Error e

                Loaded lb ->
                    case c of
                        NotAsked ->
                            NotAsked

                        Loading ->
                            Loading

                        Error e ->
                            Error e

                        Loaded lc ->
                            Loaded (f la lb lc)


map4 :
    (a -> b -> c -> d -> e)
    -> RemoteData a
    -> RemoteData b
    -> RemoteData c
    -> RemoteData d
    -> RemoteData e
map4 f a b c d =
    map2
        (\( av, bv ) ( cv, dv ) ->
            f av bv cv dv
        )
        (map2 Tuple.pair a b)
        (map2 Tuple.pair c d)


map5 :
    (a -> b -> c -> d -> e -> f)
    -> RemoteData a
    -> RemoteData b
    -> RemoteData c
    -> RemoteData d
    -> RemoteData e
    -> RemoteData f
map5 f a b c d e =
    map2
        (\( av, bv ) ( cv, dv, ev ) ->
            f av bv cv dv ev
        )
        (map2 Tuple.pair a b)
        (map3 (\cv dv ev -> ( cv, dv, ev )) c d e)
