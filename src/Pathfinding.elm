module Pathfinding exposing (filterStopTimes, filterStops, filterTrips, pathfind, pathfind2)

import Angle
import Date exposing (Date)
import Dict exposing (Dict)
import GTFS
import GTFS.Tables exposing (Calendar, CalendarDate, Pathway, Stop, StopTime, Trip)
import Id exposing (Id, PathwayId, ServiceId, StopId, TripId)
import List.Extra
import SQLite.Codec
import SeqDict exposing (SeqDict)
import SeqDict.Extra
import SeqSet exposing (SeqSet)
import Time exposing (Weekday(..))


filterStops : SeqDict (Id StopId) Stop -> SeqDict (Id StopId) Stop
filterStops stops =
    let
        stations : SeqSet (Id StopId)
        stations =
            [ "Pde:09162:100" -- München Hbf - ÖBB
            , "Pit:22095:7049" -- Udine - ÖBB
            , "Pat:42:3654" -- Villach Hbf - ÖBB
            , "Pat:45:50002" -- Salzburg Hbf - ÖBB
            , "Pde:09162:5" -- München Ost - ÖBB
            , "Pit:22095:7068" -- Tarvisio - ÖBB

            -- , "Pde:09172:42293" -- Freilassing - ÖBB
            , "Pit:22095:7068" -- Tarvisio Boscoverde - ÖBB

            -- "Pde:09162:10" -- Pasing - ÖBB
            ]
                |> List.map Id.fromString
                |> SeqSet.fromList
    in
    stops
        |> SeqDict.toList
        |> List.filter
            (\( _, stop ) ->
                SeqSet.member stop.id stations
                    || (case stop.parent_station of
                            Nothing ->
                                False

                            Just parent_id ->
                                SeqSet.member parent_id stations
                       )
            )
        |> List.take 1000
        |> SeqDict.fromList


filterStopTimes : SeqDict (Id TripId) Trip -> SeqDict (Id StopId) Stop -> List StopTime -> List ( Id TripId, List StopTime )
filterStopTimes filteredTrips stops stopTimes =
    let
        stopIds : SeqSet (Id StopId)
        stopIds =
            SeqSet.fromList (SeqDict.keys stops)
    in
    stopTimes
        |> List.filter
            (\stopTime ->
                case stopTime.stop_id of
                    Just stop_id ->
                        SeqDict.member stopTime.trip_id filteredTrips
                            && SeqSet.member stop_id stopIds

                    Nothing ->
                        False
            )
        |> SeqDict.Extra.groupBy (\{ trip_id } -> trip_id)
        |> SeqDict.toList
        |> List.map
            (\( k, v ) ->
                ( k
                , v
                    |> List.sortBy (\stopTime -> stopTime.stop_sequence)
                )
            )


filterTrips :
    Date
    -> SeqDict (Id ServiceId) (Dict Int CalendarDate)
    -> SeqDict (Id ServiceId) Calendar
    -> SeqDict (Id TripId) Trip
    -> SeqDict (Id TripId) Trip
filterTrips today calendarDates calendars trips =
    trips
        |> SeqDict.filter
            (\_ trip ->
                case
                    calendarDates
                        |> SeqDict.get trip.service_id
                        |> Maybe.andThen (Dict.get (SQLite.Codec.dateToInt today))
                of
                    Just { exception_type } ->
                        exception_type == GTFS.ServiceAdded

                    Nothing ->
                        case SeqDict.get trip.service_id calendars of
                            Nothing ->
                                let
                                    _ =
                                        Debug.log "Could not find calendar info for service_id" trip.service_id
                                in
                                False

                            Just calendar ->
                                let
                                    correctDay : Bool
                                    correctDay =
                                        getWeekdayValueFromCalendar
                                            (Date.weekday today)
                                            calendar
                                in
                                correctDay
                                    && (Date.compare calendar.start_date today /= GT)
                                    && (Date.compare calendar.end_date today /= LT)
            )


getWeekdayValueFromCalendar : Weekday -> Calendar -> Bool
getWeekdayValueFromCalendar weekday calendar =
    case weekday of
        Mon ->
            calendar.monday

        Tue ->
            calendar.tuesday

        Wed ->
            calendar.wednesday

        Thu ->
            calendar.thursday

        Fri ->
            calendar.friday

        Sat ->
            calendar.saturday

        Sun ->
            calendar.sunday


pathfind :
    { from : Id StopId, to : Id StopId }
    -> SeqDict (Id StopId) Stop
    -> List StopTime
    -> SeqDict TripId Trip
    -> Maybe (List String)
pathfind =
    Debug.todo "pathfind"


pathfind2 :
    SeqDict (Id StopId) Stop
    -> SeqDict PathwayId Pathway
    -> Stop
    -> Stop
    -> Maybe (List String)
pathfind2 stops pathways from to =
    let
        distance : ( Float, Float ) -> ( Float, Float ) -> Float
        distance ( flon, flat ) ( tlon, tlat ) =
            -- Fast approximation for close points
            (flon - tlon) ^ 2 + (flat - tlat) ^ 2

        stopCoords : Stop -> ( Float, Float )
        stopCoords stop =
            ( Maybe.withDefault 0 <| Maybe.map Angle.inDegrees stop.lon
            , Maybe.withDefault 0 <| Maybe.map Angle.inDegrees stop.lat
            )

        getPathwaysFrom : Stop -> List { to : Stop, pathway : Pathway }
        getPathwaysFrom a =
            pathways
                |> SeqDict.foldl
                    (\_ pathway acc ->
                        case
                            ( SeqDict.get pathway.from_stop_id stops
                            , SeqDict.get pathway.to_stop_id stops
                            )
                        of
                            ( Just pathFrom, Just pathTo ) ->
                                let
                                    withStraight : List { pathway : Pathway, to : Stop }
                                    withStraight =
                                        if pathFrom == a then
                                            { pathway = pathway
                                            , to = pathTo
                                            }
                                                :: acc

                                        else
                                            acc
                                in
                                if pathTo == a && pathway.is_bidirectional then
                                    { pathway = pathway
                                    , to = pathFrom
                                    }
                                        :: withStraight

                                else
                                    withStraight

                            _ ->
                                acc
                    )
                    []
                |> List.sortBy
                    (\candidate ->
                        distance
                            (stopCoords candidate.to)
                            (stopCoords to)
                    )

        go :
            SeqDict (Id StopId) (List { to : Stop, pathway : Pathway })
            -> Stop
            -> SeqSet (Id StopId)
            -> Maybe (List a)
        go cache a visited =
            if SeqSet.member a.id visited then
                Nothing

            else if a == to then
                Just []

            else
                case SeqDict.get a.id cache of
                    Nothing ->
                        go
                            (SeqDict.insert a.id (getPathwaysFrom a) cache)
                            a
                            visited

                    Just options ->
                        options
                            |> List.Extra.findMap
                                (\pathway ->
                                    go cache pathway.to (SeqSet.insert pathway.to.id visited)
                                )
    in
    go SeqDict.empty from SeqSet.empty
