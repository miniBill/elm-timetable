module Main exposing (main)

import Browser
import Clock exposing (Clock)
import Color
import Csv.Decode
import Dagre.Attributes
import Date exposing (Date)
import Dict exposing (Dict)
import Dict.Extra
import Duration exposing (Duration, Seconds)
import GTFS exposing (Calendar, CalendarDate, Feed, Pathway, Stop, StopTime, Trip)
import Graph
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Id exposing (Id, PathwayId, ServiceId, StopId, TripId)
import IdDict exposing (IdDict)
import IdDict.Extra
import IdSet exposing (IdSet)
import List.Extra
import Quantity
import QuantityDict exposing (QuantityDict)
import RemoteData
import Render
import Render.StandardDrawers
import Render.StandardDrawers.Attributes
import Render.StandardDrawers.Types
import Set
import Table
import Time exposing (Weekday(..))
import TypedSvg exposing (circle, g, line, svg, text_, title)
import TypedSvg.Attributes exposing (class, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..))
import Types exposing (Event(..), Model, Msg(..), Station, Timetable)
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
        RemoteData.map5
            (\t st cd s c ->
                ( ( t |> Dict.values |> List.foldl IdDict.Extra.union IdDict.empty
                  , st |> Dict.values |> List.concat
                  )
                , cd |> Dict.values |> List.foldl mergeWithUnion IdDict.empty
                , ( s |> Dict.values |> List.foldl IdDict.Extra.union IdDict.empty
                  , c |> Dict.values |> List.foldl IdDict.Extra.union IdDict.empty
                  )
                )
            )
            model.trips
            model.stopTimes
            model.calendarDates
            model.stops
            model.calendars
    of
        RemoteData.Loaded ( ( trips, stopTimes ), calendarDates, ( stops, calendars ) ) ->
            let
                filteredStops : List Stop
                filteredStops =
                    filterStops stops

                filteredTrips : IdDict TripId Trip
                filteredTrips =
                    filterTrips model.today calendarDates calendars trips

                filteredStopTimes : List (List StopTime)
                filteredStopTimes =
                    filterStopTimes filteredTrips filteredStops stopTimes

                stopName : { a | stop_id : Id StopId } -> String
                stopName stopTime =
                    case IdDict.get stopTime.stop_id stops of
                        Nothing ->
                            Id.toString stopTime.stop_id

                        Just stop ->
                            let
                                idString =
                                    Id.toString stopTime.stop_id
                            in
                            case stop.parent_station of
                                Nothing ->
                                    Maybe.withDefault idString stop.name

                                Just parent_id ->
                                    case IdDict.get parent_id stops of
                                        Nothing ->
                                            stop.name
                                                |> Maybe.withDefault idString

                                        Just parent ->
                                            parent.name
                                                |> Maybe.withDefault
                                                    (stop.name
                                                        |> Maybe.withDefault idString
                                                    )

                timetable : Timetable
                timetable =
                    filteredStopTimes
                        |> List.concatMap
                            (\trip ->
                                trip
                                    |> List.filterMap
                                        (\stopTime ->
                                            Maybe.map3
                                                (\stop_id departure_time arrival_time ->
                                                    { stop_id = stop_id
                                                    , departure_time = departure_time
                                                    , arrival_time = arrival_time
                                                    , trip_id = stopTime.trip_id
                                                    }
                                                )
                                                stopTime.stop_id
                                                stopTime.departure_time
                                                stopTime.arrival_time
                                        )
                                    |> List.foldl
                                        (\stopTime ( last, acc ) ->
                                            case last of
                                                Just previous ->
                                                    if previous.trip_id == stopTime.trip_id || True then
                                                        ( Just stopTime
                                                        , { from = stopName previous
                                                          , to = stopName stopTime
                                                          , departure = previous.departure_time
                                                          , arrival = stopTime.arrival_time
                                                          }
                                                            :: acc
                                                        )

                                                    else
                                                        ( Just stopTime, acc )

                                                Nothing ->
                                                    ( Just stopTime, acc )
                                        )
                                        ( Nothing, [] )
                                    |> Tuple.second
                            )
                        |> Dict.Extra.groupBy (\{ from, to } -> ( from, to ))
                        |> Dict.toList
                        |> List.map
                            (\( ( from, to ), links ) ->
                                { from = from
                                , to = to
                                , links =
                                    links
                                        |> Quantity.sortBy (\{ departure } -> departure)
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


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { today = Date.fromCalendarDate 2024 Time.Jul 7
      , timetable = []
      , stops = RemoteData.Loading
      , pathways = RemoteData.Loading
      , stopTimes = RemoteData.Loading
      , calendars = RemoteData.Loading
      , trips = RemoteData.Loading
      , calendarDates = RemoteData.Loading
      , from = Id.fromString "Pit:22095:7049"
      , to = Id.fromString "Pde:09162:100"
      }
    , loadData
    )


loadData : Cmd Msg
loadData =
    [ -- "de" ,
      --   "oebb-2024",
      "micotra-2024"
    ]
        |> List.concatMap
            (\feed ->
                [ getCSVId GotStops feed "stops.txt" GTFS.stopDecoder
                , getCSVId GotPathways feed "pathways.txt" GTFS.pathwayDecoder
                , getCSV GotStopTimes feed "stop_times.txt" GTFS.stopTimeDecoder
                , getCSVId GotTrips feed "trips.txt" GTFS.tripDecoder
                , getCSVId GotCalendars feed "calendar.txt" GTFS.calendarDecoder
                , getCSV GotCalendarDates feed "calendar_dates.txt" GTFS.calendarDateDecoder
                ]
            )
        |> Cmd.batch


getCSVId :
    (String -> Result Http.Error (IdDict kind { a | id : Id kind }) -> msg)
    -> String
    -> String
    -> Csv.Decode.Decoder { a | id : Id kind }
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
    List { a | id : Id kind }
    -> IdDict kind { a | id : Id kind }
toDictFromId list =
    List.foldl
        (\stop acc -> IdDict.insert stop.id stop acc)
        IdDict.empty
        list


getCSV :
    (String -> Result Http.Error (List a) -> msg)
    -> String
    -> String
    -> Csv.Decode.Decoder a
    -> Cmd msg
getCSV toMsg feed filename decoder =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.Builder.absolute [ "feeds", feed, filename ] []
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        , expect = expectCsv toMsg feed filename decoder
        }


expectCsv :
    (String -> Result Http.Error (List a) -> msg)
    -> String
    -> String
    -> Csv.Decode.Decoder a
    -> Http.Expect msg
expectCsv toMsg feed filename decoder =
    Http.expectString
        (\got ->
            case got of
                Ok res ->
                    case Csv.Decode.decodeCsv Csv.Decode.FieldNamesFromFirstRow decoder res of
                        Ok val ->
                            toMsg feed (Ok val)

                        Err err ->
                            let
                                msg : String
                                msg =
                                    "While decoding "
                                        ++ feed
                                        ++ "/"
                                        ++ filename
                                        ++ ", "
                                        ++ Csv.Decode.errorToString err
                            in
                            toMsg feed (Err (Http.BadBody msg))

                Err e ->
                    toMsg feed (Err e)
        )


view : Model -> Html Msg
view model =
    Html.div []
        [ viewGraphs model
        , Html.button
            [ Html.Events.onClick Reload ]
            [ Html.text "Reload" ]
        , Html.div
            [ Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "margin-top" "8px"
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
                                            [ Html.text "Trips not asked" ]

                                        RemoteData.Loading ->
                                            [ Html.text "Trips loading..." ]

                                        RemoteData.Loaded trips ->
                                            case model.calendars of
                                                RemoteData.Error e ->
                                                    [ Html.text (Debug.toString e) ]

                                                RemoteData.NotAsked ->
                                                    [ Html.text "Calendars not asked" ]

                                                RemoteData.Loading ->
                                                    [ Html.text "Calendars loading..." ]

                                                RemoteData.Loaded calendars ->
                                                    case model.calendarDates of
                                                        RemoteData.Error e ->
                                                            [ Html.text (Debug.toString e) ]

                                                        RemoteData.NotAsked ->
                                                            [ Html.text "Calendar dates not asked" ]

                                                        RemoteData.Loading ->
                                                            [ Html.text "Calendar dates loading..." ]

                                                        RemoteData.Loaded calendarDates ->
                                                            pathways
                                                                |> mergePair stops
                                                                |> mergePair calendars
                                                                |> mergePair trips
                                                                |> mergePair stopTimes
                                                                |> mergePair calendarDates
                                                                |> Dict.toList
                                                                |> List.map (viewFeed model.today)
        ]


mergePair :
    Dict comparable a
    -> Dict comparable b
    -> Dict comparable ( a, b )
mergePair l r =
    Dict.merge
        (\_ _ acc -> acc)
        (\k le re acc ->
            Dict.insert
                k
                ( le, re )
                acc
        )
        (\_ _ acc -> acc)
        l
        r
        Dict.empty


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


viewFeed :
    Date
    ->
        ( Feed
        , ( IdDict ServiceId (Dict Int CalendarDate)
          , ( List StopTime
            , ( IdDict TripId Trip
              , ( IdDict ServiceId Calendar
                , ( IdDict StopId Stop
                  , IdDict PathwayId Pathway
                  )
                )
              )
            )
          )
        )
    -> Html msg
viewFeed today ( feed, ( calendarDates, ( stopTimes, ( trips, ( calendars, ( stops, pathways ) ) ) ) ) ) =
    let
        filteredStops : List Stop
        filteredStops =
            filterStops stops

        stopIds : IdSet StopId
        stopIds =
            filteredStops
                |> List.map .id
                |> IdSet.fromList

        filteredPathways : List Pathway
        filteredPathways =
            pathways
                |> IdDict.values
                |> List.filter
                    (\walkway ->
                        IdSet.member walkway.from_stop_id stopIds
                            && IdSet.member walkway.to_stop_id stopIds
                    )
    in
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "8px"
        ]
        [ Html.text feed

        -- , pathfinder stops pathways
        , viewStops stops filteredStops
        , if False then
            viewPathways stops filteredPathways

          else
            Html.text ""
        , if False then
            let
                filteredTrips : IdDict TripId Trip
                filteredTrips =
                    filterTrips today calendarDates calendars trips

                filteredStopTimes : List (List StopTime)
                filteredStopTimes =
                    filterStopTimes filteredTrips filteredStops stopTimes
            in
            filteredStopTimes
                |> List.filterMap
                    (\tripStops ->
                        if List.length tripStops < 2 then
                            Nothing

                        else
                            Just (viewStopTimes stops filteredTrips tripStops)
                    )
                |> Html.div []

          else
            Html.text ""
        ]


filterStopTimes : IdDict TripId Trip -> List Stop -> List StopTime -> List (List StopTime)
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
        |> Dict.Extra.groupBy (\{ trip_id } -> Id.toString trip_id)
        |> Dict.values
        |> List.map
            (\v ->
                v
                    |> List.sortBy (\stopTime -> stopTime.stop_sequence)
            )


filterStops : IdDict StopId Stop -> List Stop
filterStops stops =
    let
        stations =
            [ "Pde:09162:100" -- München Hbf - ÖBB
            , "Pit:22095:7049" -- Udine - ÖBB
            , "Pat:42:3654" -- Villach Hbf - ÖBB
            , "Pat:45:50002" -- Salzburg Hbf - ÖBB
            , "Pde:09162:5" -- München Ost - ÖBB
            , "Pit:22095:7068" -- Tarvisio - ÖBB
            , "Pde:09172:42293" -- Freilassing - ÖBB

            -- "Pde:09162:10" -- Pasing
            --     , "de:09162:6:40:81"
            --     , "de:09162:6_G"
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
                    || True
            )
        |> List.take 1000


viewStops : IdDict StopId Stop -> List Stop -> Html msg
viewStops stops filteredStops =
    let
        stopName : Id StopId -> String
        stopName id =
            case IdDict.get id stops of
                Nothing ->
                    Id.toString id

                Just found ->
                    [ found.name
                    , found.description
                    , Maybe.map (\n -> "(" ++ n ++ ")") found.platform_code
                    ]
                        |> List.filterMap identity
                        |> String.join " - "

        viewStop : Stop -> Html msg
        viewStop stop =
            [ Table.id stop.id
            , Table.maybe Table.string stop.code
            , Table.maybe Table.string stop.name
            , Table.maybe Table.string stop.tts_name
            , Table.maybe Table.string stop.description
            , Table.maybe Table.angle stop.lat
            , Table.maybe Table.angle stop.lon
            , Table.maybe Table.id stop.zone_id
            , Table.maybe Table.url stop.url
            , Table.debug stop.location_type
            , Table.maybe (Table.string << stopName) stop.parent_station
            , Table.maybe Table.string stop.timezone
            , Table.maybe Table.debug stop.wheelchair_boarding
            , Table.maybe Table.id stop.level_id
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


viewPathways : IdDict StopId Stop -> List Pathway -> Html msg
viewPathways stops filteredPathways =
    let
        stop : Id StopId -> String
        stop id =
            case IdDict.get id stops of
                Nothing ->
                    Id.toString id

                Just found ->
                    [ found.name
                    , found.description
                    , Maybe.map (\n -> "(" ++ n ++ ")") found.platform_code
                    ]
                        |> List.filterMap identity
                        |> String.join " - "

        stopIds : List (Id StopId)
        stopIds =
            filteredPathways
                |> List.concatMap
                    (\pathway ->
                        [ pathway.from_stop_id
                        , pathway.to_stop_id
                        ]
                    )
                |> IdSet.fromList
                |> IdSet.toList

        stopIdToNodeId : IdDict StopId Int
        stopIdToNodeId =
            stopIds
                |> List.indexedMap (\i id -> ( id, i ))
                |> IdDict.fromList

        graph : Graph.Graph (Id StopId) ()
        graph =
            let
                edges : List (Graph.Edge ())
                edges =
                    filteredPathways
                        |> List.concatMap
                            (\pathway ->
                                case
                                    ( IdDict.get pathway.from_stop_id stopIdToNodeId
                                    , IdDict.get pathway.to_stop_id stopIdToNodeId
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

                nodes : List (Graph.Node (Id StopId))
                nodes =
                    stopIds
                        |> List.indexedMap
                            (\i id ->
                                { id = i
                                , label = id
                                }
                            )
            in
            Graph.fromNodesAndEdges nodes edges

        viewPathway : Pathway -> Html msg
        viewPathway pathway =
            [ Table.id pathway.id
            , Table.string (stop pathway.from_stop_id)
            , Table.string (stop pathway.to_stop_id)
            , Table.debug pathway.mode
            , Table.debug pathway.is_bidirectional
            , Table.maybe Table.length pathway.length
            , Table.maybe Table.duration pathway.traversal_time
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


viewStopTimes : IdDict StopId Stop -> IdDict TripId Trip -> List StopTime -> Html msg
viewStopTimes stops filteredTrips filteredStopTimes =
    let
        trip : Id TripId -> String
        trip id =
            case IdDict.get id filteredTrips of
                Nothing ->
                    Id.toString id

                Just found ->
                    [ found.short_name
                    , Just (Id.toString id)
                    ]
                        |> List.filterMap identity
                        |> String.join " - "

        stop : Id StopId -> String
        stop id =
            case IdDict.get id stops of
                Nothing ->
                    Id.toString id

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
            , Table.maybe Table.clock stopTime.arrival_time
            , Table.maybe Table.clock stopTime.departure_time
            , Table.maybe Table.string (Maybe.map stop stopTime.stop_id)
            , Table.maybe Table.id stopTime.location_group_id
            , Table.maybe Table.id stopTime.location_id
            , Table.int stopTime.stop_sequence
            , Table.maybe Table.string stopTime.stop_headsign
            , Table.maybe Table.clock stopTime.start_pickup_drop_off_window
            , Table.maybe Table.clock stopTime.end_pickup_drop_off_window
            , Table.maybe Table.debug stopTime.pickup_type
            , Table.maybe Table.debug stopTime.drop_off_type
            , Table.maybe Table.debug stopTime.continuous_pickup
            , Table.maybe Table.debug stopTime.continuous_drop_off
            , Table.maybe Table.float stopTime.shape_dist_traveled
            , Table.maybe Table.bool stopTime.timepoint
            , Table.maybe Table.id stopTime.pickup_booking_rule_id
            , Table.maybe Table.id stopTime.drop_off_booking_rule_id
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


viewGraphs : Model -> Html msg
viewGraphs model =
    let
        fullHeight : Float
        fullHeight =
            timesHeight * 2 + lineHeight * toFloat (Dict.size stations - 1)

        liftTime :
            (Clock -> Clock -> Clock)
            -> Maybe Clock
            -> Clock
            -> Clock
        liftTime op acc e =
            case acc of
                Nothing ->
                    e

                Just v ->
                    op v e

        addStation :
            Station
            -> Clock
            -> Event
            ->
                Dict
                    Station
                    { min : Clock
                    , max : Clock
                    , events : QuantityDict Int Seconds Event
                    }
            ->
                Dict
                    Station
                    { min : Clock
                    , max : Clock
                    , events : QuantityDict Int Seconds Event
                    }
        addStation station time event dict =
            let
                new =
                    case Dict.get station dict of
                        Nothing ->
                            { min = time
                            , max = time
                            , events =
                                QuantityDict.singleton time event
                            }

                        Just existing ->
                            { min = liftTime Quantity.min (Just existing.min) time
                            , max = liftTime Quantity.max (Just existing.max) time
                            , events =
                                QuantityDict.insert time event existing.events
                            }
            in
            Dict.insert station new dict

        times : List ( Clock, Clock )
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

        timeRange : { minTime : Maybe Clock, maxTime : Maybe Clock }
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
                { min : Clock
                , max : Clock
                , events : QuantityDict Int Seconds Event
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
                , { events : QuantityDict Int Seconds Event
                  , min : Clock
                  , max : Clock
                  }
                )
        sortedStations =
            stations
                |> Dict.toList
                |> List.sortBy (\( station, _ ) -> stationOrder station)
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
                                        [ title []
                                            [ text
                                                (Clock.toHumanString link.from
                                                    ++ " - "
                                                    ++ Clock.toHumanString link.to
                                                )
                                            ]
                                        ]
                                )
                    )

        endpointsViews : List (Svg msg)
        endpointsViews =
            model.timetable
                |> List.concatMap
                    (\{ from, to, links } ->
                        links
                            |> List.concatMap
                                (\link ->
                                    [ circle
                                        [ class [ "endpoint" ]
                                        , cx <| timeToX timeRange link.from
                                        , cy <| stationToY from
                                        ]
                                        []
                                    , circle
                                        [ class [ "endpoint" ]
                                        , cx <| timeToX timeRange link.to
                                        , cy <| stationToY to
                                        ]
                                        []
                                    ]
                                )
                    )

        timesViews : List (Svg msg)
        timesViews =
            case ( timeRange.minTime, timeRange.maxTime ) of
                ( Just minTime, Just maxTime ) ->
                    let
                        from : Int
                        from =
                            ceiling <| Duration.inHours <| Quantity.toFloatQuantity minTime

                        to : Int
                        to =
                            floor <| Duration.inHours <| Quantity.toFloatQuantity maxTime
                    in
                    List.range from to
                        |> List.map
                            (\hour ->
                                let
                                    time : Clock
                                    time =
                                        Clock.fromHoursMinutesSeconds hour 0 0

                                    timeX : Float
                                    timeX =
                                        timeToX timeRange time

                                    verticalLine : Svg msg
                                    verticalLine =
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
                                                    [ Clock.toHumanString time
                                                        |> text
                                                    ]
                                        in
                                        [ inner AnchorStart (timesHeight - pushUp)
                                        , inner AnchorEnd (fullHeight - timesHeight + pushUp)
                                        ]

                                    pushUp =
                                        timesHeight / 2
                                in
                                g
                                    [ transform [ Translate timeX 0 ] ]
                                    (if hour == from then
                                        verticalLine :: label

                                     else
                                        verticalLine :: label
                                    )
                            )

                _ ->
                    []

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

                    .endpoint {
                        fill: green;
                        r: 5px;
                    }
                    """
                ]
    in
    Html.div
        [ Html.Attributes.style "width" "100%"
        , Html.Attributes.style "overflow" "scroll"
        ]
        [ svg
            [ Html.Attributes.style "width" "100%"
            , Html.Attributes.style "padding" "1vmin 1vmin"
            , Html.Attributes.style "min-width" "1500px"
            , viewBox -5 -5 (fullWidth + 10) (fullHeight + 10)
            ]
            (styleNode :: stationsViews ++ linksViews ++ timesViews ++ endpointsViews)
        ]


stationOrder : Station -> Int
stationOrder station =
    case station of
        "Trieste Centrale" ->
            1

        "Monfalcone" ->
            2

        "Trieste Airport" ->
            3

        "Cervignano - Aquileia - Grado" ->
            4

        "Palmanova" ->
            5

        "Udine" ->
            6

        "Udine stazione" ->
            6

        "Tarvisio Citta Boscoverde" ->
            12

        "Villach Hauptbahnhof" ->
            18

        "Salzburg Hauptbahnhof" ->
            19

        "Freilassing Bahnhof" ->
            20

        "München Hauptbahnhof" ->
            21

        _ ->
            999


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
    let
        _ =
            -- Calculate this from timetable
            Debug.todo
    in
    250


timeToX :
    { minTime : Maybe Clock
    , maxTime : Maybe Clock
    }
    -> Clock
    -> Float
timeToX { minTime, maxTime } time =
    case ( minTime, maxTime ) of
        ( Just min, Just max ) ->
            namesWidth
                + tableHorizontalMargin
                + (fullWidth - namesWidth - tableHorizontalMargin * 2)
                * Quantity.ratio
                    (Quantity.toFloatQuantity <| Quantity.difference time min)
                    (Quantity.toFloatQuantity <| Quantity.difference max min)

        _ ->
            -- This never happens but we're going to force a mislayout if the assumptions are wrong
            namesWidth / 2


viewStation :
    { minTime : Maybe Clock, maxTime : Maybe Clock }
    -> Dict Station Int
    -> ( Station, { events : QuantityDict Int Seconds Event, min : Clock, max : Clock } )
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
                go : List ( Clock, Event ) -> List (Svg msg) -> List (Svg msg)
                go queue acc =
                    case queue of
                        [] ->
                            List.reverse acc

                        ( at, _ ) :: tail ->
                            let
                                nextDeparture : Maybe Clock
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
                                        duration : Duration
                                        duration =
                                            Clock.duration dep at

                                        timeString : String
                                        timeString =
                                            let
                                                rawMins : Int
                                                rawMins =
                                                    Duration.inMinutes duration
                                                        |> floor
                                            in
                                            if rawMins > 60 then
                                                String.fromInt (rawMins // 60)
                                                    ++ "h "
                                                    ++ String.padLeft 2 '0' (String.fromInt (modBy 60 rawMins))
                                                    ++ "m"

                                            else
                                                String.fromInt rawMins ++ "m"
                                    in
                                    go tail
                                        (line
                                            [ class [ "wait" ]
                                            , x1 <| timeToX timeRange at
                                            , x2 <| timeToX timeRange dep
                                            , y1 stationY
                                            , y2 stationY
                                            , stroke (Paint (waitTimeToColor duration))
                                            ]
                                            [ title []
                                                [ text timeString
                                                ]
                                            ]
                                            :: acc
                                        )
            in
            go (QuantityDict.toList events) []
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
        GotStops feed res ->
            ( { model | stops = mergeFeed feed res model.stops }, Cmd.none )

        GotPathways feed res ->
            ( { model | pathways = mergeFeed feed res model.pathways }, Cmd.none )

        GotTrips feed res ->
            ( { model | trips = mergeFeed feed res model.trips }, Cmd.none )

        GotStopTimes feed res ->
            ( { model | stopTimes = mergeFeed feed res model.stopTimes }, Cmd.none )

        GotCalendars feed res ->
            ( { model | calendars = mergeFeed feed res model.calendars }, Cmd.none )

        GotCalendarDates feed res ->
            let
                grouped : Result Http.Error (IdDict ServiceId (Dict Int CalendarDate))
                grouped =
                    Result.map
                        (\dates ->
                            dates
                                |> List.foldl
                                    (\calendarDate acc ->
                                        IdDict.insert calendarDate.service_id
                                            (IdDict.get calendarDate.service_id acc
                                                |> Maybe.withDefault Dict.empty
                                                |> Dict.insert (GTFS.dateToInt calendarDate.date)
                                                    calendarDate
                                            )
                                            acc
                                    )
                                    IdDict.empty
                        )
                        res
            in
            ( { model | calendarDates = mergeFeed feed grouped model.calendarDates }, Cmd.none )

        Reload ->
            ( model, loadData )


mergeFeed :
    Feed
    -> Result Http.Error a
    -> RemoteData.RemoteData (Dict Feed a)
    -> RemoteData.RemoteData (Dict Feed a)
mergeFeed feed res data =
    case res of
        Err e ->
            RemoteData.Error e

        Ok val ->
            let
                existing : Dict Feed a
                existing =
                    case data of
                        RemoteData.Loaded loaded ->
                            loaded

                        _ ->
                            Dict.empty
            in
            RemoteData.Loaded (Dict.insert feed val existing)


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none
