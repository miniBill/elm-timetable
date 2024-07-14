module Types exposing (Event(..), Model, Msg(..), Station, Timetable)

import Clock exposing (Clock)
import Date exposing (Date)
import Dict exposing (Dict)
import GTFS exposing (Calendar, CalendarDate, Feed, Pathway, Stop, StopTime, Trip)
import Http
import Id exposing (Id, PathwayId, ServiceId, StopId, TripId)
import IdDict exposing (IdDict)
import RemoteData exposing (RemoteData)


type alias Model =
    { today : Date
    , timetable : Timetable
    , stops : RemoteData (Dict Feed (IdDict StopId Stop))
    , pathways : RemoteData (Dict Feed (IdDict PathwayId Pathway))
    , stopTimes : RemoteData (Dict Feed (List StopTime))
    , calendars : RemoteData (Dict Feed (IdDict ServiceId Calendar))
    , trips : RemoteData (Dict Feed (IdDict TripId Trip))
    , calendarDates : RemoteData (Dict Feed (IdDict ServiceId (Dict Int CalendarDate)))
    , from : Id StopId
    , to : Id StopId
    }


type alias Timetable =
    List
        { from : Station
        , to : Station
        , links : List { from : Clock, label : String, to : Clock }
        }


type alias Station =
    String


type Event
    = Arrival
    | Departure


type Msg
    = Reload
    | GotStops Feed (Result Http.Error (IdDict StopId Stop))
    | GotPathways Feed (Result Http.Error (IdDict PathwayId Pathway))
    | GotStopTimes Feed (Result Http.Error (List StopTime))
    | GotTrips Feed (Result Http.Error (IdDict TripId Trip))
    | GotCalendars Feed (Result Http.Error (IdDict ServiceId Calendar))
    | GotCalendarDates Feed (Result Http.Error (List CalendarDate))
