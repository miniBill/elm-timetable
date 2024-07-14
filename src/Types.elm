module Types exposing (Event(..), Link, Model, Msg(..), Station, Timetable)

import Clock exposing (Clock)
import Date exposing (Date)
import Feed exposing (Feed)
import Http
import Id exposing (FeedId, Id, StopId)
import IdDict exposing (IdDict)
import RemoteData exposing (RemoteData)


type alias Model =
    { today : Date
    , timetable : Timetable
    , feeds : IdDict FeedId (RemoteData Feed)
    , from : Id StopId
    , to : Id StopId
    }


type alias Timetable =
    List
        { from : Station
        , to : Station
        , links : List Link
        }


type alias Link =
    { from : Clock
    , label : String
    , train : String
    , to : Clock
    }


type alias Station =
    String


type Event
    = Arrival
    | Departure


type Msg
    = Reload
    | GotFeed (Id FeedId) (Result Http.Error Feed)
