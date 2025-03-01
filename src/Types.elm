module Types exposing (Event(..), Link, Model, Msg(..), Station, Timetable)

import Clock exposing (Clock)
import Date exposing (Date)
import Feed exposing (Feed)
import Http
import Id exposing (FeedId, Id, StopId)
import RemoteData exposing (RemoteData)
import SeqDict exposing (SeqDict)


type alias Model =
    { today : Date
    , timetable : Timetable
    , feeds : SeqDict (Id FeedId) (RemoteData Feed)
    , from : Id StopId
    , to : Id StopId
    , search : String
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
    | Search String
