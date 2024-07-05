module RemoteData exposing (RemoteData(..))

import Http


type RemoteData a
    = NotAsked
    | Loading
    | Loaded a
    | Error Http.Error
