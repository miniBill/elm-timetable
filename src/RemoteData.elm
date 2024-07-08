module RemoteData exposing (RemoteData(..), map)

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
