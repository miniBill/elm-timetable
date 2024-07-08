module RemoteData exposing (RemoteData(..), map, map3)

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
