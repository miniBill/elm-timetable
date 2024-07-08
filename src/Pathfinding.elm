module Pathfinding exposing (pathfind, pathfind2)

import Dict exposing (Dict)
import GTFS exposing (Id, Pathway, Stop, StopTime, Trip)


pathfind :
    { from : Id, to : Id }
    -> Dict Id Stop
    -> Dict Id StopTime
    -> Dict Id Trip
    -> Maybe (List String)
pathfind =
    Debug.todo "pathfind"


pathfind2 :
    Dict Id Stop
    -> Dict Id Pathway
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
            ( Maybe.withDefault 0 stop.lon
            , Maybe.withDefault 0 stop.lat
            )

        getPathwaysFrom : Stop -> List { to : Stop, pathway : Pathway }
        getPathwaysFrom a =
            pathways
                |> Dict.foldl
                    (\_ pathway acc ->
                        case
                            ( Dict.get pathway.from_stop_id stops
                            , Dict.get pathway.to_stop_id stops
                            )
                        of
                            ( Just pathFrom, Just pathTo ) ->
                                let
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
            Dict Id (List { to : Stop, pathway : Pathway })
            -> Stop
            -> Set String
            -> Maybe (List a)
        go cache a visited =
            if Set.member a.id visited then
                Nothing

            else if a == to then
                Just []

            else
                case Dict.get a.id cache of
                    Nothing ->
                        go
                            (Dict.insert a.id (getPathwaysFrom a) cache)
                            a
                            visited

                    Just options ->
                        options
                            |> List.Extra.findMap
                                (\pathway ->
                                    go cache pathway.to (Set.insert pathway.to.id visited)
                                )
    in
    go Dict.empty from Set.empty
