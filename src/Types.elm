module Types exposing (Model, Msg(..), OEvent(..), OStation, OTimetable, OViewMode(..))

import Dict exposing (Dict)
import GTFS exposing (Feed, Id, Pathway, Stop, StopTime)
import Http
import RemoteData exposing (RemoteData)
import Time


type alias Model =
    { timetable : OTimetable
    , mode : OViewMode
    , stops : RemoteData (Dict Feed (Dict Id Stop))
    , pathways : RemoteData (Dict Feed (Dict Id Pathway))
    , stopTimes : RemoteData (Dict Feed (List StopTime))
    }


type OViewMode
    = ViewSimple


type alias OTimetable =
    List
        { from : OStation
        , to : OStation
        , links : List { from : Time.Posix, to : Time.Posix }
        }


type alias OStation =
    String


type OEvent
    = Arrival
    | Departure


type Msg
    = OViewMode OViewMode
    | Reload
    | GotStops Feed (Result Http.Error (Dict Id Stop))
    | GotPathways Feed (Result Http.Error (Dict Id Pathway))
    | GotStopTimes Feed (Result Http.Error (List StopTime))
