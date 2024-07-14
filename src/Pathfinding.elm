module Pathfinding exposing (filterStopTimes, filterStops, filterTrips, pathfind, pathfind2)

import Angle
import Date exposing (Date)
import Dict exposing (Dict)
import GTFS exposing (Calendar, CalendarDate, Pathway, Stop, StopTime, Trip)
import Id exposing (Id, PathwayId, ServiceId, StopId, TripId)
import IdDict exposing (IdDict)
import IdDict.Extra
import IdSet exposing (IdSet)
import List.Extra
import Set exposing (Set)
import Time exposing (Weekday(..))


filterStops : IdDict StopId Stop -> List Stop
filterStops stops =
    let
        stations : Set String
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
                |> Set.fromList
    in
    stops
        |> IdDict.values
        |> List.filter
            (\stop ->
                Set.member (Id.toString stop.id) stations
                    || (case stop.parent_station of
                            Nothing ->
                                False

                            Just parent_id ->
                                Set.member (Id.toString parent_id) stations
                       )
            )
        |> List.take 1000


filterStopTimes : IdDict TripId Trip -> List Stop -> List StopTime -> List ( Id TripId, List StopTime )
filterStopTimes filteredTrips filteredStops stopTimes =
    let
        stopIds : IdSet StopId
        stopIds =
            IdSet.fromList (List.map (\stop -> stop.id) filteredStops)
    in
    stopTimes
        |> List.filter
            (\stopTime ->
                case stopTime.stop_id of
                    Just stop_id ->
                        IdDict.member stopTime.trip_id filteredTrips
                            && IdSet.member stop_id stopIds

                    Nothing ->
                        False
            )
        |> IdDict.Extra.groupBy (\{ trip_id } -> trip_id)
        |> IdDict.toList
        |> List.map
            (\( k, v ) ->
                ( k
                , v
                    |> List.sortBy (\stopTime -> stopTime.stop_sequence)
                )
            )


filterTrips :
    Date
    -> IdDict ServiceId (Dict Int CalendarDate)
    -> IdDict ServiceId Calendar
    -> IdDict TripId Trip
    -> IdDict TripId Trip
filterTrips today calendarDates calendars trips =
    trips
        |> IdDict.filter
            (\_ trip ->
                case
                    calendarDates
                        |> IdDict.get trip.service_id
                        |> Maybe.andThen (Dict.get (GTFS.dateToInt today))
                of
                    Just { exception_type } ->
                        exception_type == GTFS.ServiceAdded

                    Nothing ->
                        case IdDict.get trip.service_id calendars of
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
                                        case Date.weekday today of
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
                                in
                                correctDay
                                    && (Date.compare calendar.start_date today /= GT)
                                    && (Date.compare calendar.end_date today /= LT)
            )


pathfind :
    { from : Id StopId, to : Id StopId }
    -> IdDict StopId Stop
    -> List StopTime
    -> IdDict TripId Trip
    -> Maybe (List String)
pathfind =
    Debug.todo "pathfind"


pathfind2 :
    IdDict StopId Stop
    -> IdDict PathwayId Pathway
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
                |> IdDict.foldl
                    (\_ pathway acc ->
                        case
                            ( IdDict.get pathway.from_stop_id stops
                            , IdDict.get pathway.to_stop_id stops
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
            IdDict StopId (List { to : Stop, pathway : Pathway })
            -> Stop
            -> IdSet StopId
            -> Maybe (List a)
        go cache a visited =
            if IdSet.member a.id visited then
                Nothing

            else if a == to then
                Just []

            else
                case IdDict.get a.id cache of
                    Nothing ->
                        go
                            (IdDict.insert a.id (getPathwaysFrom a) cache)
                            a
                            visited

                    Just options ->
                        options
                            |> List.Extra.findMap
                                (\pathway ->
                                    go cache pathway.to (IdSet.insert pathway.to.id visited)
                                )
    in
    go IdDict.empty from IdSet.empty
