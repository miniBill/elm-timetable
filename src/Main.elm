module Main exposing (main)

import Browser
import Csv.Decode
import Date exposing (Date)
import Dict
import Feed exposing (Feed)
import GTFS exposing (Pathway, Stop, StopTime, Trip)
import Http
import Id exposing (FeedId, Id, StopId, TripId)
import IdDict exposing (IdDict)
import IdSet exposing (IdSet)
import Pathfinding
import Platform exposing (Task)
import RemoteData
import Table
import Task
import Theme
import Time
import Timetable
import Types exposing (Model, Msg(..))
import Ui exposing (Element)
import Ui.Table
import Url.Builder


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = \model -> Ui.layout [] (view model)
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
        IdDict.foldl
            (\_ -> RemoteData.map2 Feed.merge)
            (RemoteData.Loaded Feed.empty)
            model.feeds
    of
        RemoteData.Loaded data ->
            { model | timetable = Timetable.build model.today data }

        _ ->
            model


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        feeds : List (Id FeedId)
        feeds =
            [ -- Id.fromString "de" ,
              Id.fromString "oebb-2024"
            , Id.fromString "micotra-2024"
            ]
    in
    ( { today = Date.fromCalendarDate 2024 Time.Jul 9
      , timetable = []
      , feeds =
            feeds
                |> List.map (\feed -> ( feed, RemoteData.Loading ))
                |> IdDict.fromList
      , from = Id.fromString "Pde:09162:100"
      , to = Id.fromString "Pit:22095:7049"
      }
    , feeds
        |> List.map
            (\feed ->
                Task.map2
                    (\( stops, pathways, stopTimes ) ( trips, calendars, calendarDates ) ->
                        let
                            q : Feed
                            q =
                                { stops = stops
                                , pathways = pathways
                                , stopTimes = stopTimes
                                , trips = trips
                                , calendars = calendars
                                , calendarDates =
                                    calendarDates
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
                                }
                        in
                        q
                    )
                    (Task.map3 (\l m r -> ( l, m, r ))
                        (getCSVId feed "stops.txt" GTFS.stopDecoder)
                        (getCSVId feed "pathways.txt" GTFS.pathwayDecoder)
                        (getCSV feed "stop_times.txt" GTFS.stopTimeDecoder)
                    )
                    (Task.map3 (\l m r -> ( l, m, r ))
                        (getCSVId feed "trips.txt" GTFS.tripDecoder)
                        (getCSVId feed "calendar.txt" GTFS.calendarDecoder)
                        (getCSV feed "calendar_dates.txt" GTFS.calendarDateDecoder)
                    )
                    |> Task.attempt (GotFeed feed)
            )
        |> Cmd.batch
    )


getCSVId :
    Id FeedId
    -> String
    -> Csv.Decode.Decoder { a | id : Id kind }
    -> Task Http.Error (IdDict kind { a | id : Id kind })
getCSVId feed filename decoder =
    getCSV feed filename decoder
        |> Task.map toDictFromId


toDictFromId :
    List { a | id : Id kind }
    -> IdDict kind { a | id : Id kind }
toDictFromId list =
    List.foldl
        (\stop acc -> IdDict.insert stop.id stop acc)
        IdDict.empty
        list


getCSV :
    Id FeedId
    -> String
    -> Csv.Decode.Decoder a
    -> Task Http.Error (List a)
getCSV feed filename decoder =
    Http.task
        { method = "GET"
        , headers = []
        , url = Url.Builder.absolute [ "feeds", Id.toString feed, filename ] []
        , timeout = Nothing
        , body = Http.emptyBody
        , resolver = csvResolver feed filename decoder
        }


csvResolver :
    Id FeedId
    -> String
    -> Csv.Decode.Decoder a
    -> Http.Resolver Http.Error (List a)
csvResolver feed filename decoder =
    Http.stringResolver
        (\got ->
            case got of
                Http.GoodStatus_ _ res ->
                    case Csv.Decode.decodeCsv Csv.Decode.FieldNamesFromFirstRow decoder res of
                        Ok val ->
                            Ok val

                        Err err ->
                            let
                                msg : String
                                msg =
                                    "While decoding "
                                        ++ Id.toString feed
                                        ++ "/"
                                        ++ filename
                                        ++ ", "
                                        ++ Csv.Decode.errorToString err
                            in
                            Err (Http.BadBody msg)

                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ { statusCode } _ ->
                    Err (Http.BadStatus statusCode)
        )


view : Model -> Element Msg
view model =
    let
        shared : List (Element Msg)
        shared =
            [ Timetable.view model.timetable
                |> Ui.html
                |> Ui.el
                    [ Ui.scrollableX
                    , Theme.padding
                    ]
            , Timetable.viewGraph model.timetable
                |> Ui.html
                |> Ui.el
                    [ Ui.scrollableX
                    , Ui.paddingWith { left = Theme.rhythm, bottom = Theme.rhythm, right = Theme.rhythm, top = 0 }
                    ]
            , Ui.el
                [ Ui.scrollableX
                , Ui.paddingWith { left = Theme.rhythm, bottom = Theme.rhythm, right = Theme.rhythm, top = 0 }
                ]
                (Theme.button []
                    { onPress = Reload
                    , label = Ui.text "Reload"
                    }
                )
            ]

        feedViews : List (Element msg)
        feedViews =
            model.feeds
                |> IdDict.toList
                |> List.map
                    (\( feedId, feed ) ->
                        let
                            label : String
                            label =
                                Id.toString feedId
                        in
                        case feed of
                            RemoteData.Error e ->
                                Ui.text (Debug.toString e)

                            RemoteData.NotAsked ->
                                Ui.text ("Feed " ++ label ++ " not asked")

                            RemoteData.Loading ->
                                Ui.text ("Feed " ++ label ++ " loading...")

                            RemoteData.Loaded loaded ->
                                loaded
                                    |> viewFeed model.today feedId
                                    |> Ui.el
                                        [ Ui.scrollableX
                                        , Ui.paddingWith { left = Theme.rhythm, bottom = Theme.rhythm, right = Theme.rhythm, top = 0 }
                                        ]
                    )
    in
    Theme.column [] (shared ++ feedViews)


viewFeed :
    Date
    -> Id FeedId
    -> Feed
    -> Element msg
viewFeed today feed { calendarDates, stopTimes, trips, calendars, stops, pathways } =
    let
        filteredStops : List Stop
        filteredStops =
            Pathfinding.filterStops stops
    in
    Theme.column []
        [ Ui.text (Id.toString feed)

        -- , pathfinder stops filteredPathways
        , viewStops stops filteredStops
        , if False then
            let
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
            viewPathways stops filteredPathways

          else
            Ui.none
        , if False then
            let
                filteredTrips : IdDict TripId Trip
                filteredTrips =
                    Pathfinding.filterTrips today calendarDates calendars trips

                filteredStopTimes : List ( Id TripId, List StopTime )
                filteredStopTimes =
                    Pathfinding.filterStopTimes filteredTrips filteredStops stopTimes
            in
            filteredStopTimes
                |> List.filterMap
                    (\( _, tripStops ) ->
                        if List.length tripStops < 2 then
                            Nothing

                        else
                            Just (viewStopTimes stops filteredTrips tripStops)
                    )
                |> Theme.column []

          else
            Ui.none
        ]


viewStops : IdDict StopId Stop -> List Stop -> Element msg
viewStops stops filteredStops =
    let
        maybeColumn :
            String
            -> (Stop -> Maybe value)
            -> (value -> Ui.Table.Cell msg)
            -> Maybe (Ui.Table.Column globalState rowState Stop msg)
        maybeColumn header prop viewItem =
            if List.any (\value -> prop value /= Nothing) data then
                Theme.tableColumn header prop (Table.maybe viewItem)

            else
                Nothing

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

        columns : List (Maybe (Ui.Table.Column globalState rowState Stop msg))
        columns =
            [ Theme.tableColumn "id" .id Table.id
            , maybeColumn "code" .code Table.string
            , maybeColumn "name" .name Table.string
            , maybeColumn "tts_name" .tts_name Table.string
            , maybeColumn "description" .description Table.string
            , maybeColumn "lat" .lat Table.angle
            , maybeColumn "lon" .lon Table.angle
            , maybeColumn "zone_id" .zone_id Table.id
            , maybeColumn "url" .url Table.url
            , Theme.tableColumn "location_type" .location_type (Table.string << GTFS.locationTypeToString)
            , maybeColumn "parent_station" .parent_station (Table.string << stopName)
            , maybeColumn "timezone" .timezone Table.string
            , maybeColumn "wheelchair_boarding" .wheelchair_boarding Table.accessibility
            , maybeColumn "level_id" .level_id Table.id
            , maybeColumn "platform_code" .platform_code Table.string
            ]

        data : List Stop
        data =
            filteredStops
                |> List.filter (\stop -> stop.parent_station == Nothing)
    in
    Theme.table [] columns data


viewPathways : IdDict StopId Stop -> List Pathway -> Element msg
viewPathways stops filteredPathways =
    let
        maybeColumn :
            String
            -> (Pathway -> Maybe value)
            -> (value -> Ui.Table.Cell msg)
            -> Maybe (Ui.Table.Column globalState rowState Pathway msg)
        maybeColumn header prop viewItem =
            if List.any (\value -> prop value /= Nothing) data then
                Theme.tableColumn header prop (Table.maybe viewItem)

            else
                Nothing

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

        data : List Pathway
        data =
            filteredPathways

        columns : List (Maybe (Ui.Table.Column globalState rowState Pathway msg))
        columns =
            [ Theme.tableColumn "id" .id Table.id
            , Theme.tableColumn "from" .from_stop_id (Table.string << stopName)
            , Theme.tableColumn "to" .to_stop_id (Table.string << stopName)
            , Theme.tableColumn "mode" .mode Table.pathwayMode
            , Theme.tableColumn "is_bidirectional" .is_bidirectional Table.bool
            , maybeColumn "length" .length Table.length
            , maybeColumn "traversal_time" .traversal_time Table.duration
            , maybeColumn "stair_count" .stair_count Table.int
            , maybeColumn "max_slope" .max_slope Table.float
            , maybeColumn "min_width" .min_width Table.length
            , maybeColumn "signposted_as" .signposted_as Table.string
            , maybeColumn "reversed_signposted_as" .reversed_signposted_as Table.string
            ]
    in
    Theme.column []
        [ Theme.table [] columns data
        , filteredPathways
            |> List.concatMap
                (\{ from_stop_id, to_stop_id, is_bidirectional } ->
                    if is_bidirectional then
                        [ ( from_stop_id, to_stop_id )
                        , ( to_stop_id, from_stop_id )
                        ]

                    else
                        [ ( from_stop_id, to_stop_id ) ]
                )
            |> Timetable.viewDAG stopName
            |> Ui.html
            |> Ui.el
                [ Theme.padding
                , Ui.scrollableX
                ]
        ]


viewStopTimes : IdDict StopId Stop -> IdDict TripId Trip -> List StopTime -> Element msg
viewStopTimes stops filteredTrips filteredStopTimes =
    let
        maybeColumn :
            String
            -> (StopTime -> Maybe value)
            -> (value -> Ui.Table.Cell msg)
            -> Maybe (Ui.Table.Column globalState rowState StopTime msg)
        maybeColumn header prop viewItem =
            if List.any (\value -> prop value /= Nothing) data then
                Theme.tableColumn header prop (Table.maybe viewItem)

            else
                Nothing

        data : List StopTime
        data =
            filteredStopTimes

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

        columns : List (Maybe (Ui.Table.Column globalState rowState StopTime msg))
        columns =
            [ Theme.tableColumn "trip_id" .trip_id (Table.string << trip)
            , maybeColumn "arrival_time" .arrival_time Table.clock
            , maybeColumn "departure_time" .departure_time Table.clock
            , maybeColumn "stop_id" .stop_id (Table.string << stopName)
            , maybeColumn "location_group_id" .location_group_id Table.id
            , maybeColumn "location_id" .location_id Table.id
            , Theme.tableColumn "stop_sequence" .stop_sequence Table.int
            , maybeColumn "stop_headsign" .stop_headsign Table.string
            , maybeColumn "start_pickup_drop_off_window" .start_pickup_drop_off_window Table.clock
            , maybeColumn "end_pickup_drop_off_window" .end_pickup_drop_off_window Table.clock
            , maybeColumn "pickup_type" .pickup_type Table.pickupDropOffType
            , maybeColumn "drop_off_type" .drop_off_type Table.pickupDropOffType
            , maybeColumn "continuous_pickup" .continuous_pickup Table.pickupDropOffType
            , maybeColumn "continuous_drop_off" .continuous_drop_off Table.pickupDropOffType
            , maybeColumn "shape_dist_traveled" .shape_dist_traveled Table.float
            , maybeColumn "timepoint" .timepoint Table.bool
            , maybeColumn "pickup_booking_rule_id" .pickup_booking_rule_id Table.id
            , maybeColumn "drop_off_booking_rule_id" .drop_off_booking_rule_id Table.id
            ]
    in
    Theme.table [] columns data


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFeed feed (Ok res) ->
            ( { model | feeds = IdDict.insert feed (RemoteData.Loaded res) model.feeds }, Cmd.none )

        GotFeed feed (Err e) ->
            ( { model | feeds = IdDict.insert feed (RemoteData.Error e) model.feeds }, Cmd.none )

        Reload ->
            init {}


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none
