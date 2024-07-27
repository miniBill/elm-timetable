module Feed exposing (Feed, empty, merge)

import Dict exposing (Dict)
import GTFS.Tables exposing (Calendar, CalendarDate, Pathway, Stop, StopTime, Trip)
import Id exposing (PathwayId, ServiceId, StopId, TripId)
import IdDict exposing (IdDict)
import IdDict.Extra


type alias Feed =
    { stops : IdDict StopId Stop
    , pathways : IdDict PathwayId Pathway
    , stopTimes : List StopTime
    , calendars : IdDict ServiceId Calendar
    , trips : IdDict TripId Trip
    , calendarDates : IdDict ServiceId (Dict Int CalendarDate)
    }


empty : Feed
empty =
    { stops = IdDict.empty
    , pathways = IdDict.empty
    , stopTimes = []
    , calendars = IdDict.empty
    , trips = IdDict.empty
    , calendarDates = IdDict.empty
    }


merge : Feed -> Feed -> Feed
merge l r =
    { trips = IdDict.Extra.union l.trips r.trips
    , stopTimes = l.stopTimes ++ r.stopTimes
    , calendarDates = mergeWithUnion l.calendarDates r.calendarDates
    , stops = IdDict.Extra.union l.stops r.stops
    , calendars = IdDict.Extra.union l.calendars r.calendars
    , pathways = IdDict.Extra.union l.pathways r.pathways
    }


mergeWithUnion :
    IdDict kind (Dict comparable v)
    -> IdDict kind (Dict comparable v)
    -> IdDict kind (Dict comparable v)
mergeWithUnion l r =
    IdDict.Extra.merge
        (\_ _ acc -> acc)
        (\k le re acc ->
            IdDict.insert
                k
                (Dict.union le re)
                acc
        )
        (\_ _ acc -> acc)
        l
        r
        IdDict.empty
