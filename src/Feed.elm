module Feed exposing (Feed, empty, merge)

import Dict exposing (Dict)
import GTFS.Tables exposing (Calendar, CalendarDate, Pathway, Stop, StopTime, Trip)
import Id exposing (Id, PathwayId, ServiceId, StopId, TripId)
import SeqDict exposing (SeqDict)


type alias Feed =
    { stops : SeqDict (Id StopId) Stop
    , pathways : SeqDict (Id PathwayId) Pathway
    , stopTimes : List StopTime
    , calendars : SeqDict (Id ServiceId) Calendar
    , trips : SeqDict (Id TripId) Trip
    , calendarDates : SeqDict (Id ServiceId) (Dict Int CalendarDate)
    }


empty : Feed
empty =
    { stops = SeqDict.empty
    , pathways = SeqDict.empty
    , stopTimes = []
    , calendars = SeqDict.empty
    , trips = SeqDict.empty
    , calendarDates = SeqDict.empty
    }


merge : Feed -> Feed -> Feed
merge l r =
    { trips = SeqDict.union l.trips r.trips
    , stopTimes = l.stopTimes ++ r.stopTimes
    , calendarDates = mergeWithUnion l.calendarDates r.calendarDates
    , stops = SeqDict.union l.stops r.stops
    , calendars = SeqDict.union l.calendars r.calendars
    , pathways = SeqDict.union l.pathways r.pathways
    }


mergeWithUnion :
    SeqDict kind (Dict comparable v)
    -> SeqDict kind (Dict comparable v)
    -> SeqDict kind (Dict comparable v)
mergeWithUnion l r =
    SeqDict.merge
        (\_ _ acc -> acc)
        (\k le re acc ->
            SeqDict.insert
                k
                (Dict.union le re)
                acc
        )
        (\_ _ acc -> acc)
        l
        r
        SeqDict.empty
