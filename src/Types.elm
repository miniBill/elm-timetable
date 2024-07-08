module Types exposing (Event(..), Model, Msg(..), Station, Timetable, ViewMode(..))

import Dict exposing (Dict)
import GTFS exposing (Feed, Id, Pathway, Stop, StopTime, Time, Trip)
import Http
import RemoteData exposing (RemoteData)


type alias Model =
    { timetable : Timetable
    , mode : ViewMode
    , stops : RemoteData (Dict Feed (Dict Id Stop))
    , pathways : RemoteData (Dict Feed (Dict Id Pathway))
    , stopTimes : RemoteData (Dict Feed (List StopTime))
    , trips : RemoteData (Dict Feed (Dict Id Trip))
    }


type ViewMode
    = ViewSimple


type alias Timetable =
    List
        { from : Station
        , to : Station
        , links : List { from : Time, to : Time }
        }


type alias Station =
    String


type Event
    = Arrival
    | Departure


type Msg
    = OViewMode ViewMode
    | Reload
    | GotStops Feed (Result Http.Error (Dict Id Stop))
    | GotPathways Feed (Result Http.Error (Dict Id Pathway))
    | GotStopTimes Feed (Result Http.Error (List StopTime))
    | GotTrips Feed (Result Http.Error (Dict Id Trip))
