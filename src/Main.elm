module Main exposing (main)

import Browser
import Color
import Csv.Decode
import Dagre.Attributes
import Data
import Dict exposing (Dict)
import Dict.Extra
import Duration exposing (Duration)
import GTFS exposing (Feed, Id, LocationType(..), Pathway, PathwayMode(..), Stop, StopTime, Time, Trip)
import Graph
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import List.Extra
import Quantity
import RemoteData
import Render
import Render.StandardDrawers
import Render.StandardDrawers.Attributes
import Render.StandardDrawers.Types
import Set exposing (Set)
import Table
import TypedSvg exposing (g, line, svg, text_, title)
import TypedSvg.Attributes exposing (class, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..), Paint(..), Transform(..))
import Types exposing (Event(..), Model, Msg(..), Station, Timetable, ViewMode(..))
import Url.Builder


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update =
            \msg model ->
                let
                    ( newModel, cmd ) =
                        update msg model
                in
                ( rebuildTimetable newModel, cmd )
        , subscriptions = subscriptions
        }


rebuildTimetable : Model -> Model
rebuildTimetable model =
    case
        RemoteData.map3
            (\t st s ->
                ( t |> Dict.values |> List.foldl Dict.union Dict.empty
                , st |> Dict.values |> List.concat
                , s |> Dict.values |> List.foldl Dict.union Dict.empty
                )
            )
            model.trips
            model.stopTimes
            model.stops
    of
        RemoteData.Loaded ( trips, stopTimes, stops ) ->
            let
                filteredStops : List Stop
                filteredStops =
                    filterStops stops

                filteredStopTimes : Dict ( Id, Id ) (List StopTime)
                filteredStopTimes =
                    filterStopTimes trips filteredStops stopTimes

                stopName : { a | stop_id : Id } -> Id
                stopName stopTime =
                    case Dict.get stopTime.stop_id stops of
                        Nothing ->
                            stopTime.stop_id

                        Just stop ->
                            case stop.parent_station of
                                Nothing ->
                                    Maybe.withDefault stopTime.stop_id stop.name

                                Just parent_id ->
                                    case Dict.get parent_id stops of
                                        Nothing ->
                                            Maybe.withDefault stopTime.stop_id stop.name

                                        Just parent ->
                                            Maybe.withDefault (Maybe.withDefault stopTime.stop_id stop.name) parent.name

                timetable : Timetable
                timetable =
                    filteredStopTimes
                        |> Dict.values
                        |> List.concatMap
                            (\trip ->
                                let
                                    converted :
                                        List
                                            { from : Id
                                            , to : Id
                                            , departure : Time
                                            , arrival : Time
                                            }
                                    converted =
                                        trip
                                            |> List.filterMap
                                                (\stopTime ->
                                                    Maybe.map3
                                                        (\stop_id departure_time arrival_time ->
                                                            { stop_id = stop_id
                                                            , departure_time = departure_time
                                                            , arrival_time = arrival_time
                                                            }
                                                        )
                                                        stopTime.stop_id
                                                        stopTime.departure_time
                                                        stopTime.arrival_time
                                                )
                                            |> List.foldl
                                                (\stopTime ( last, acc ) ->
                                                    case last of
                                                        Nothing ->
                                                            ( Just stopTime, acc )

                                                        Just previous ->
                                                            ( Just stopTime
                                                            , { from = stopName previous
                                                              , to = stopName stopTime
                                                              , departure = previous.departure_time
                                                              , arrival = stopTime.arrival_time
                                                              }
                                                                :: acc
                                                            )
                                                )
                                                ( Nothing, [] )
                                            |> Tuple.second
                                in
                                converted
                            )
                        |> Dict.Extra.groupBy (\{ from, to } -> ( from, to ))
                        |> Dict.toList
                        |> List.map
                            (\( ( from, to ), links ) ->
                                { from = from
                                , to = to
                                , links =
                                    links
                                        |> List.map
                                            (\{ departure, arrival } ->
                                                { from = departure
                                                , to = arrival
                                                }
                                            )
                                }
                            )
            in
            { model | timetable = timetable }

        _ ->
            model


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { timetable = [] -- Data.villachToUdine
      , mode = ViewSimple
      , stops = RemoteData.Loading
      , pathways = RemoteData.Loading
      , stopTimes = RemoteData.Loading
      , trips = RemoteData.Loading
      }
    , loadData
    )


loadData : Cmd Msg
loadData =
    Data.feeds
        |> List.concatMap
            (\feed ->
                [ getCSVId GotStops feed "stops.txt" GTFS.stopDecoder
                , getCSVId GotPathways feed "pathways.txt" GTFS.pathwayDecoder
                , getCSV GotStopTimes feed "stop_times.txt" GTFS.stopTimeDecoder
                , getCSVId GotTrips feed "trips.txt" GTFS.tripDecoder
                ]
            )
        |> Cmd.batch


getCSVId :
    (String -> Result Http.Error (Dict Id { a | id : Id }) -> msg)
    -> String
    -> String
    -> Csv.Decode.Decoder { a | id : Id }
    -> Cmd msg
getCSVId toMsg feed filename decoder =
    getCSV
        (\_ raw ->
            raw
                |> Result.map toDictFromId
                |> toMsg feed
        )
        feed
        filename
        decoder


toDictFromId :
    List { a | id : Id }
    -> Dict Id { a | id : Id }
toDictFromId list =
    List.foldl
        (\stop acc -> Dict.insert stop.id stop acc)
        Dict.empty
        list


getCSV : (String -> Result Http.Error (List a) -> msg) -> String -> String -> Csv.Decode.Decoder a -> Cmd msg
getCSV toMsg feed filename decoder =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.Builder.absolute [ "feeds", feed, filename ] []
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        , expect =
            Http.expectString
                (\got ->
                    got
                        |> Result.andThen
                            (\res ->
                                Csv.Decode.decodeCsv Csv.Decode.FieldNamesFromFirstRow decoder res
                                    |> Result.mapError
                                        (\err ->
                                            Http.BadBody
                                                ("While decoding "
                                                    ++ feed
                                                    ++ "/"
                                                    ++ filename
                                                    ++ ", "
                                                    ++ Csv.Decode.errorToString err
                                                )
                                        )
                            )
                        |> toMsg feed
                )
        }


view : Model -> Html Msg
view model =
    Html.div []
        [ case model.mode of
            ViewSimple ->
                viewSimple model
        , Html.button
            [ Html.Events.onClick Reload ]
            [ Html.text "Reload" ]
        , Html.div
            [ Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "padding" "8px"
            ]
          <|
            case model.stops of
                RemoteData.Error e ->
                    [ Html.text (Debug.toString e) ]

                RemoteData.NotAsked ->
                    [ Html.text "Stops not asked" ]

                RemoteData.Loading ->
                    [ Html.text "Stops loading..." ]

                RemoteData.Loaded stops ->
                    case model.pathways of
                        RemoteData.Error e ->
                            [ Html.text (Debug.toString e) ]

                        RemoteData.NotAsked ->
                            [ Html.text "Pathways not asked" ]

                        RemoteData.Loading ->
                            [ Html.text "Pathways loading..." ]

                        RemoteData.Loaded pathways ->
                            case model.stopTimes of
                                RemoteData.Error e ->
                                    [ Html.text (Debug.toString e) ]

                                RemoteData.NotAsked ->
                                    [ Html.text "Stop times not asked" ]

                                RemoteData.Loading ->
                                    [ Html.text "Stop times loading..." ]

                                RemoteData.Loaded stopTimes ->
                                    case model.trips of
                                        RemoteData.Error e ->
                                            [ Html.text (Debug.toString e) ]

                                        RemoteData.NotAsked ->
                                            [ Html.text "trips not asked" ]

                                        RemoteData.Loading ->
                                            [ Html.text "trips loading..." ]

                                        RemoteData.Loaded trips ->
                                            let
                                                stopsAndPathways :
                                                    Dict
                                                        Feed
                                                        ( Dict Id Stop, Dict Id Pathway )
                                                stopsAndPathways =
                                                    Dict.merge
                                                        (\_ _ acc -> acc)
                                                        (\k stop pathway acc ->
                                                            Dict.insert
                                                                k
                                                                ( stop, pathway )
                                                                acc
                                                        )
                                                        (\_ _ acc -> acc)
                                                        stops
                                                        pathways
                                                        Dict.empty

                                                stopTimesAndTrips =
                                                    Dict.merge
                                                        (\_ _ acc -> acc)
                                                        (\k stopTime trip acc ->
                                                            Dict.insert
                                                                k
                                                                ( stopTime, trip )
                                                                acc
                                                        )
                                                        (\_ _ acc -> acc)
                                                        stopTimes
                                                        trips
                                                        Dict.empty
                                            in
                                            Dict.merge
                                                (\_ _ acc -> acc)
                                                (\k l r acc ->
                                                    Dict.insert
                                                        k
                                                        ( l, r )
                                                        acc
                                                )
                                                (\_ _ acc -> acc)
                                                stopTimesAndTrips
                                                stopsAndPathways
                                                Dict.empty
                                                |> Dict.toList
                                                |> List.map viewFeed
        ]


viewFeed : ( Feed, ( ( List StopTime, Dict Id Trip ), ( Dict Id Stop, Dict Id Pathway ) ) ) -> Html msg
viewFeed ( feed, ( ( stopTimes, trips ), ( stops, pathways ) ) ) =
    let
        filteredStops : List Stop
        filteredStops =
            filterStops stops

        -- filteredPathways : List Pathway
        -- filteredPathways =
        --     pathways
        --         |> Dict.values
        --         |> List.filter
        --             (\walkway ->
        --                 Set.member walkway.from_stop_id stopIds
        --                     && Set.member walkway.to_stop_id stopIds
        --             )
        filteredStopTimes =
            filterStopTimes trips filteredStops stopTimes
    in
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "8px"
        ]
        [ Html.text feed

        -- , pathfinder stops pathways
        , viewStops stops filteredStops

        -- , viewPathways stops filteredPathways
        , filteredStopTimes
            |> Dict.values
            |> List.filterMap
                (\tripStops ->
                    if List.length tripStops < 2 then
                        Nothing

                    else
                        Just (viewStopTimes stops trips tripStops)
                )
            |> Html.div []
        ]


filterStopTimes : Dict Id Trip -> List Stop -> List StopTime -> Dict ( Id, Id ) (List StopTime)
filterStopTimes trips filteredStops stopTimes =
    let
        stopIds : Set Id
        stopIds =
            Set.fromList (List.map (\stop -> stop.id) filteredStops)
    in
    stopTimes
        |> List.filter
            (\stopTime ->
                case Dict.get stopTime.trip_id trips of
                    Nothing ->
                        False

                    Just trip ->
                        let
                            defaulted : String
                            defaulted =
                                Maybe.withDefault "" trip.short_name
                        in
                        if
                            String.startsWith "S " defaulted
                                || String.startsWith "NJ " defaulted
                                || String.startsWith "EN " defaulted
                        then
                            False

                        else
                            case stopTime.stop_id of
                                Just id ->
                                    Set.member id stopIds

                                Nothing ->
                                    False
            )
        |> Dict.Extra.groupBy (\stopTime -> stopTime.trip_id)
        |> Dict.toList
        |> Dict.Extra.groupBy
            (\( trip_id, block ) ->
                case Dict.get trip_id trips of
                    Nothing ->
                        ( trip_id, "" )

                    Just trip ->
                        case trip.block_id of
                            Just id ->
                                ( trip.route_id, id )

                            Nothing ->
                                ( trip_id, "" )
            )
        |> Dict.map
            (\_ v ->
                v
                    |> List.concatMap Tuple.second
                    |> List.sortBy
                        (\stopTime ->
                            case stopTime.arrival_time of
                                Nothing ->
                                    stopTime.stop_sequence

                                Just arrival ->
                                    Quantity.unwrap arrival
                        )
            )


filterStops : Dict Id Stop -> List Stop
filterStops stops =
    stops
        |> Dict.values
        |> List.filter
            (\stop ->
                -- let
                --     defaulted : String
                --     defaulted =
                --         Maybe.withDefault "" stop.name
                -- in
                -- List.member defaulted
                --     [ "München Hbf"
                --     , "München Hauptbahnhof"
                --     ]
                -- || String.contains "Isartor" defaulted
                List.member stop.id
                    [ "Pde:09162:100" -- München Hbf - ÖBB
                    , "Pit:22095:7049" -- Udine - ÖBB
                    , "Pat:42:3654" -- Villach Hbf - ÖBB
                    , "Pat:45:50002" -- Salzburg Hbf - ÖBB

                    -- , "Pde:09162:5" -- München Ost - ÖBB
                    , "Pit:22095:7068" -- Tarvisio - ÖBB
                    , "Pde:09172:42293" -- Freilassing - ÖBB

                    -- "Pde:09162:10" -- Pasing
                    --     , "de:09162:6:40:81"
                    --     , "de:09162:6_G"
                    ]
                    || List.member stop.parent_station
                        [ --  Just "Pde:09162:100" -- München Hbf
                          -- , Just "Pde:09162:10" -- München Pasing
                          -- , Just "de:09162:6_G"
                          Just "Pde:09162:100" -- München Hbf - ÖBB
                        , Just "Pit:22095:7049" -- Udine - ÖBB
                        , Just "Pat:42:3654" -- Villach Hbf - ÖBB
                        , Just "Pat:45:50002" -- Salzburg Hbf - ÖBB

                        -- , Just "Pde:09162:5" -- München Ost - ÖBB
                        , Just "Pit:22095:7068" -- Tarvisio - ÖBB
                        , Just "Pde:09172:42293" -- Freilassing - ÖBB
                        ]
            )
        |> List.take 1000


viewStops : Dict Id Stop -> List Stop -> Html msg
viewStops stops filteredStops =
    let
        stopName : Id -> String
        stopName id =
            case Dict.get id stops of
                Nothing ->
                    id

                Just found ->
                    [ found.name
                    , found.description
                    , Maybe.map (\n -> "(" ++ n ++ ")") found.platform_code
                    ]
                        |> List.filterMap identity
                        |> String.join " - "

        viewStop : Stop -> Html msg
        viewStop stop =
            [ Table.string stop.id
            , Table.maybe Table.string stop.code
            , Table.maybe Table.string stop.name
            , Table.maybe Table.string stop.tts_name
            , Table.maybe Table.string stop.description
            , Table.maybe Table.angle stop.lat
            , Table.maybe Table.angle stop.lon
            , Table.maybe Table.string stop.zone_id
            , Table.maybe Table.url stop.url
            , Table.debug stop.location_type
            , Table.maybe (Table.string << stopName) stop.parent_station
            , Table.maybe Table.string stop.timezone
            , Table.maybe Table.debug stop.wheelchair_boarding
            , Table.maybe Table.string stop.level_id
            , Table.maybe Table.string stop.platform_code
            ]
                |> Html.tr []
    in
    filteredStops
        |> List.filter (\stop -> stop.parent_station == Nothing)
        |> List.map viewStop
        |> (::)
            ([ "id"
             , "code"
             , "name"
             , "tts_name"
             , "description"
             , "lat"
             , "lon"
             , "zone_id"
             , "url"
             , "location_type"
             , "parent_station"
             , "timezone"
             , "wheelchair_boarding"
             , "level_id"
             , "platform_code"
             ]
                |> List.map (\col -> Html.th [] [ Html.text col ])
                |> Html.tr []
            )
        |> Html.table
            [ Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "padding" "8px"
            ]


viewPathways : Dict Id Stop -> List Pathway -> Html msg
viewPathways stops filteredPathways =
    let
        stop : Id -> String
        stop id =
            case Dict.get id stops of
                Nothing ->
                    id

                Just found ->
                    [ found.name
                    , found.description
                    , Maybe.map (\n -> "(" ++ n ++ ")") found.platform_code
                    ]
                        |> List.filterMap identity
                        |> String.join " - "

        stopIds : List Id
        stopIds =
            filteredPathways
                |> List.concatMap
                    (\pathway ->
                        [ pathway.from_stop_id
                        , pathway.to_stop_id
                        ]
                    )
                |> Set.fromList
                |> Set.toList

        stopIdToNodeId : Dict Id Int
        stopIdToNodeId =
            stopIds
                |> List.indexedMap (\i id -> ( id, i ))
                |> Dict.fromList

        graph : Graph.Graph Id ()
        graph =
            let
                edges : List (Graph.Edge ())
                edges =
                    filteredPathways
                        |> List.concatMap
                            (\pathway ->
                                case
                                    ( Dict.get pathway.from_stop_id stopIdToNodeId
                                    , Dict.get pathway.to_stop_id stopIdToNodeId
                                    )
                                of
                                    ( Just fromId, Just toId ) ->
                                        if pathway.is_bidirectional then
                                            [ { from = fromId
                                              , to = toId
                                              , label = ()
                                              }
                                            , { from = toId
                                              , to = fromId
                                              , label = ()
                                              }
                                            ]

                                        else
                                            [ { from = fromId
                                              , to = toId
                                              , label = ()
                                              }
                                            ]

                                    _ ->
                                        []
                            )

                nodes : List (Graph.Node String)
                nodes =
                    stopIds
                        |> List.indexedMap (\i id -> { id = i, label = id })
            in
            Graph.fromNodesAndEdges nodes edges

        viewPathway : Pathway -> Html msg
        viewPathway pathway =
            [ Table.string pathway.id
            , Table.string (stop pathway.from_stop_id)
            , Table.string (stop pathway.to_stop_id)
            , Table.debug pathway.mode
            , Table.debug pathway.is_bidirectional
            , Table.maybe Table.length pathway.length
            , Table.maybe Table.seconds pathway.traversal_time
            , Table.maybe Table.int pathway.stair_count
            , Table.maybe Table.float pathway.max_slope
            , Table.maybe Table.length pathway.min_width
            , Table.maybe Table.string pathway.signposted_as
            , Table.maybe Table.string pathway.reversed_signposted_as
            ]
                |> Html.tr []
    in
    Html.div []
        [ filteredPathways
            |> List.map viewPathway
            |> (::)
                ([ "id"
                 , "from"
                 , "to"
                 , "mode"
                 , "is_bidirectional"
                 , "length"
                 , "traversal_time"
                 , "stair_count"
                 , "max_slope"
                 , "min_width"
                 , "signposted_as"
                 , "reversed_signposted_as"
                 ]
                    |> List.map (\col -> Html.th [] [ Html.text col ])
                    |> Html.tr []
                )
            |> Html.table
                [ Html.Attributes.style "border" "1px solid black"
                , Html.Attributes.style "padding" "8px"
                ]
        , if False then
            Render.draw
                [ Dagre.Attributes.rankDir Dagre.Attributes.LR

                -- , Dagre.Attributes.widthDict
                --     (stopIds
                --         |> List.map
                --             (\id ->
                --                 ( Dict.get id stopIdToNodeId
                --                     |> Maybe.withDefault -1
                --                 , (10 * String.length (stop id))
                --                     |> toFloat
                --                 )
                --             )
                --         |> Dict.fromList
                --     )
                , Dagre.Attributes.width 300
                ]
                [ Render.nodeDrawer
                    (Render.StandardDrawers.svgDrawNode
                        [ Render.StandardDrawers.Attributes.title (\{ label } -> stop label)
                        , Render.StandardDrawers.Attributes.label (\{ label } -> stop label)
                        ]
                    )
                , Render.edgeDrawer
                    (Render.StandardDrawers.svgDrawEdge
                        [ Render.StandardDrawers.Attributes.arrowHead
                            Render.StandardDrawers.Types.Vee
                        , Render.StandardDrawers.Attributes.strokeWidth (\_ -> 4)
                        ]
                    )
                , Render.style "width: 100%;max-height:100vh;max-width:100vw"
                ]
                graph

          else
            Html.text ""
        ]


viewStopTimes : Dict Id Stop -> Dict Id Trip -> List StopTime -> Html msg
viewStopTimes stops trips filteredStopTimes =
    let
        trip : Id -> String
        trip id =
            case Dict.get id trips of
                Nothing ->
                    id

                Just found ->
                    [ found.short_name
                    , found.block_id
                    , Just id
                    ]
                        |> List.filterMap identity
                        |> String.join " - "

        stop : Id -> String
        stop id =
            case Dict.get id stops of
                Nothing ->
                    id

                Just found ->
                    [ found.name
                    , found.description
                    , Maybe.map (\n -> "(" ++ n ++ ")") found.platform_code
                    ]
                        |> List.filterMap identity
                        |> String.join " - "

        viewStopTime : StopTime -> Html msg
        viewStopTime stopTime =
            [ Table.string (trip stopTime.trip_id)
            , Table.maybe Table.time stopTime.arrival_time
            , Table.maybe Table.time stopTime.departure_time
            , Table.maybe Table.string (Maybe.map stop stopTime.stop_id)
            , Table.maybe Table.string stopTime.location_group_id
            , Table.maybe Table.string stopTime.location_id
            , Table.int stopTime.stop_sequence
            , Table.maybe Table.string stopTime.stop_headsign
            , Table.maybe Table.time stopTime.start_pickup_drop_off_window
            , Table.maybe Table.time stopTime.end_pickup_drop_off_window
            , Table.maybe Table.debug stopTime.pickup_type
            , Table.maybe Table.debug stopTime.drop_off_type
            , Table.maybe Table.debug stopTime.continuous_pickup
            , Table.maybe Table.debug stopTime.continuous_drop_off
            , Table.maybe Table.float stopTime.shape_dist_traveled
            , Table.maybe Table.bool stopTime.timepoint
            , Table.maybe Table.string stopTime.pickup_booking_rule_id
            , Table.maybe Table.string stopTime.drop_off_booking_rule_id
            ]
                |> Html.tr []
    in
    Html.div []
        [ filteredStopTimes
            |> List.map viewStopTime
            |> (::)
                ([ "trip_id"
                 , "arrival_time"
                 , "departure_time"
                 , "stop_id"
                 , "location_group_id"
                 , "location_id"
                 , "stop_sequence"
                 , "stop_headsign"
                 , "start_pickup_drop_off_window"
                 , "end_pickup_drop_off_window"
                 , "pickup_type"
                 , "drop_off_type"
                 , "continuous_pickup"
                 , "continuous_drop_off"
                 , "shape_dist_traveled"
                 , "timepoint"
                 , "pickup_booking_rule_id"
                 , "drop_off_booking_rule_id"
                 ]
                    |> List.map (\col -> Html.th [] [ Html.text col ])
                    |> Html.tr []
                )
            |> Html.table
                [ Html.Attributes.style "border" "1px solid black"
                , Html.Attributes.style "padding" "8px"
                ]
        ]


viewSimple : Model -> Html msg
viewSimple model =
    let
        fullHeight : Float
        fullHeight =
            timesHeight * 2 + lineHeight * toFloat (Dict.size stations - 1)

        liftTime :
            (Time -> Time -> Time)
            -> Maybe Time
            -> Time
            -> Time
        liftTime op acc e =
            case acc of
                Nothing ->
                    e

                Just v ->
                    op v e

        addStation :
            Station
            -> Time
            -> Event
            ->
                Dict
                    Station
                    { min : Time
                    , max : Time
                    , events : Dict Int Event
                    }
            ->
                Dict
                    Station
                    { min : Time
                    , max : Time
                    , events : Dict Int Event
                    }
        addStation station time event dict =
            let
                new =
                    case Dict.get station dict of
                        Nothing ->
                            { min = time
                            , max = time
                            , events =
                                Dict.singleton (Quantity.unwrap time) event
                            }

                        Just existing ->
                            { min = liftTime Quantity.min (Just existing.min) time
                            , max = liftTime Quantity.max (Just existing.max) time
                            , events =
                                Dict.insert (Quantity.unwrap time) event existing.events
                            }
            in
            Dict.insert station new dict

        times : List ( Time, Time )
        times =
            model.timetable
                |> List.concatMap
                    (\{ links } ->
                        List.map
                            (\{ from, to } ->
                                ( from, to )
                            )
                            links
                    )

        timeRange : { minTime : Maybe Time, maxTime : Maybe Time }
        timeRange =
            List.foldl
                (\( from, to ) acc ->
                    { minTime =
                        Just <| liftTime Quantity.min acc.minTime from
                    , maxTime =
                        Just <| liftTime Quantity.max acc.maxTime to
                    }
                )
                { minTime = Nothing
                , maxTime = Nothing
                }
                times

        stations :
            Dict
                Station
                { min : Time
                , max : Time
                , events : Dict Int Event
                }
        stations =
            model.timetable
                |> List.foldl
                    (\{ from, to, links } acc ->
                        links
                            |> List.foldl
                                (\link iacc ->
                                    iacc
                                        |> addStation from link.from Departure
                                        |> addStation to link.to Arrival
                                )
                                acc
                    )
                    Dict.empty

        sortedStations :
            List
                ( Station
                , { events : Dict Int Event
                  , min : Time
                  , max : Time
                  }
                )
        sortedStations =
            stations
                |> Dict.toList
                |> List.sortBy
                    (\( station, { min, max } ) ->
                        if String.contains "Udine" station then
                            0

                        else if String.contains "Tarvisio" station then
                            1

                        else if String.contains "Villach" station then
                            2

                        else if String.contains "Salzburg" station then
                            3

                        else if String.contains "Freilassing" station then
                            4

                        else if String.contains "München Ost" station then
                            5

                        else if String.contains "München Hauptbahnhof" station then
                            6

                        else
                            999
                     -- ( Quantity.unwrap min, -(Quantity.unwrap max) )
                    )
                |> List.reverse

        stationPositions : Dict Station Int
        stationPositions =
            sortedStations
                |> List.indexedMap
                    (\i ( name, _ ) -> ( name, timesHeight + i * lineHeight ))
                |> Dict.fromList

        stationsViews : List (Svg msg)
        stationsViews =
            List.map
                (viewStation timeRange stationPositions)
                sortedStations

        stationToY : Station -> Float
        stationToY station =
            Dict.get station stationPositions
                |> Maybe.withDefault -1
                |> toFloat

        linksViews : List (Svg msg)
        linksViews =
            model.timetable
                |> List.concatMap
                    (\{ from, to, links } ->
                        links
                            |> List.map
                                (\link ->
                                    line
                                        [ class [ "link" ]
                                        , x1 <| timeToX timeRange link.from
                                        , x2 <| timeToX timeRange link.to
                                        , y1 <| stationToY from
                                        , y2 <| stationToY to
                                        ]
                                        []
                                )
                    )

        timesViews : List (Svg msg)
        timesViews =
            times
                |> List.concatMap
                    (\( from, to ) ->
                        [ Quantity.unwrap from
                        , Quantity.unwrap to
                        ]
                    )
                |> Set.fromList
                |> Set.toList
                |> List.foldr
                    (\t ( last, acc ) ->
                        let
                            time : Time
                            time =
                                Quantity.unsafe t

                            timeX : Float
                            timeX =
                                timeToX timeRange time

                            vline : Svg msg
                            vline =
                                line
                                    [ class [ "grid" ]
                                    , x1 0
                                    , x2 0
                                    , y1 (timesHeight - pushUp)
                                    , y2 (fullHeight - timesHeight + pushUp)
                                    ]
                                    []

                            label : List (Svg msg)
                            label =
                                let
                                    inner anchor transformation =
                                        text_
                                            [ textAnchor anchor
                                            , transform
                                                [ Translate 5 transformation
                                                , Rotate 90 0 0
                                                ]
                                            ]
                                            [ GTFS.timeToString time
                                                |> String.left 5
                                                |> text
                                            ]
                                in
                                [ inner AnchorStart (timesHeight - pushUp)
                                , inner AnchorEnd (fullHeight - timesHeight + pushUp)
                                ]

                            pushUp =
                                case last of
                                    Nothing ->
                                        timesHeight / 2

                                    Just lastTime ->
                                        if
                                            Quantity.difference time lastTime
                                                |> Quantity.greaterThan
                                                    (Quantity.unsafe (30 * 60))
                                        then
                                            timesHeight / 2

                                        else
                                            timesHeight

                            next =
                                g
                                    [ transform [ Translate timeX 0 ] ]
                                    (vline :: label)
                        in
                        ( Just time, next :: acc )
                    )
                    ( Nothing, [] )
                |> Tuple.second

        styleNode =
            TypedSvg.style []
                [ text
                    """
                    .horiz {
                        stroke: black;
                        stroke-width: 2px;
                    }

                    .link {
                        stroke: blue;
                        stroke-width: 2px;
                    }

                    .grid {
                        stroke: gray;
                        stroke-width: 1px;
                        stroke-dasharray: 4;
                    }

                    .wait {
                        stroke-width: 2px;
                    }
                    """
                ]
    in
    svg
        [ Html.Attributes.style "width" "100%"
        , Html.Attributes.style "margin-top" "5vmin"
        , Html.Attributes.style "margin-left" "5vmin"
        , Html.Attributes.style "max-height" "90vh"
        , Html.Attributes.style "max-width" "90vw"
        , viewBox -5 -5 (fullWidth + 10) (fullHeight + 10)
        ]
        (styleNode :: stationsViews ++ linksViews ++ timesViews)


fullWidth : number
fullWidth =
    1000


tableHorizontalMargin : number
tableHorizontalMargin =
    50


lineHeight : number
lineHeight =
    50


timesHeight : number
timesHeight =
    100


namesWidth : number
namesWidth =
    150


timeToX :
    { minTime : Maybe Time
    , maxTime : Maybe Time
    }
    -> Time
    -> Float
timeToX { minTime, maxTime } time =
    case ( minTime, maxTime ) of
        ( Just min, Just max ) ->
            namesWidth
                + tableHorizontalMargin
                + (fullWidth - namesWidth - tableHorizontalMargin * 2)
                * toFloat
                    (Quantity.unwrap time - Quantity.unwrap min)
                / toFloat
                    (Quantity.unwrap max - Quantity.unwrap min)

        _ ->
            -- This never happens but we're going to force a mislayout if the assumptions are wrong
            namesWidth / 2


viewStation :
    { minTime : Maybe Time, maxTime : Maybe Time }
    -> Dict Station Int
    -> ( Station, { events : Dict Int Event, min : Time, max : Time } )
    -> Svg msg
viewStation timeRange stationPositions ( name, { events } ) =
    let
        stationY : Float
        stationY =
            Dict.get name stationPositions
                |> Maybe.withDefault -1
                |> toFloat

        waitLines : List (Svg msg)
        waitLines =
            let
                go queue acc =
                    case queue of
                        [] ->
                            List.reverse acc

                        ( at, _ ) :: tail ->
                            let
                                nextDeparture : Maybe Int
                                nextDeparture =
                                    List.Extra.findMap
                                        (\( dep, kind ) ->
                                            if kind == Departure then
                                                Just dep

                                            else
                                                Nothing
                                        )
                                        tail
                            in
                            case nextDeparture of
                                Nothing ->
                                    List.reverse acc

                                Just dep ->
                                    let
                                        duration =
                                            Duration.milliseconds (toFloat (dep - at))

                                        minString : String
                                        minString =
                                            Duration.inMinutes duration
                                                |> floor
                                                |> String.fromInt
                                    in
                                    go tail
                                        (line
                                            [ class [ "wait" ]
                                            , x1 <| timeToX timeRange (Quantity.unsafe at)
                                            , x2 <| timeToX timeRange (Quantity.unsafe dep)
                                            , y1 stationY
                                            , y2 stationY
                                            , stroke (Paint (waitTimeToColor duration))
                                            ]
                                            [ title []
                                                [ text
                                                    (minString
                                                        ++ " min"
                                                    )
                                                ]
                                            ]
                                            :: acc
                                        )
            in
            go (Dict.toList events) []
    in
    g []
        ([ line
            [ class [ "horiz" ]
            , x1 namesWidth
            , x2 fullWidth
            , y1 stationY
            , y2 stationY
            ]
            []
         , text_
            [ y stationY ]
            [ text name ]
         ]
            ++ waitLines
        )


waitTimeToColor : Duration -> Color.Color
waitTimeToColor f =
    if Quantity.lessThan (Duration.minutes 10) f then
        Color.red

    else if Quantity.lessThan (Duration.minutes 60) f then
        Color.orange

    else
        Color.green


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OViewMode mode ->
            ( { model | mode = mode }, Cmd.none )

        GotStops _ (Err e) ->
            ( { model | stops = RemoteData.Error e }, Cmd.none )

        GotStops feed (Ok res) ->
            let
                existing : Dict Feed (Dict Id Stop)
                existing =
                    case model.stops of
                        RemoteData.Loaded stops ->
                            stops

                        _ ->
                            Dict.empty
            in
            ( { model | stops = RemoteData.Loaded (Dict.insert feed res existing) }, Cmd.none )

        GotPathways _ (Err e) ->
            ( { model | pathways = RemoteData.Error e }, Cmd.none )

        GotPathways feed (Ok res) ->
            let
                existing : Dict Feed (Dict Id Pathway)
                existing =
                    case model.pathways of
                        RemoteData.Loaded pathways ->
                            pathways

                        _ ->
                            Dict.empty
            in
            ( { model | pathways = RemoteData.Loaded (Dict.insert feed res existing) }, Cmd.none )

        GotTrips _ (Err e) ->
            ( { model | trips = RemoteData.Error e }, Cmd.none )

        GotTrips feed (Ok res) ->
            let
                existing : Dict Feed (Dict Id Trip)
                existing =
                    case model.trips of
                        RemoteData.Loaded trips ->
                            trips

                        _ ->
                            Dict.empty
            in
            ( { model | trips = RemoteData.Loaded (Dict.insert feed res existing) }, Cmd.none )

        GotStopTimes _ (Err e) ->
            ( { model | stopTimes = RemoteData.Error e }, Cmd.none )

        GotStopTimes feed (Ok res) ->
            let
                existing : Dict Feed (List StopTime)
                existing =
                    case model.stopTimes of
                        RemoteData.Loaded stopTimes ->
                            stopTimes

                        _ ->
                            Dict.empty
            in
            ( { model | stopTimes = RemoteData.Loaded (Dict.insert feed res existing) }, Cmd.none )

        Reload ->
            ( model, loadData )


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none
