module Main exposing (main)

import Browser
import Csv.Decode
import Date exposing (Date)
import Dict exposing (Dict)
import Dict.Extra
import GTFS exposing (Calendar, CalendarDate, Feed, Pathway, Stop, StopTime, Trip)
import Http
import Id exposing (Id, PathwayId, ServiceId, StopId, TripId)
import IdDict exposing (IdDict)
import IdDict.Extra
import IdSet exposing (IdSet)
import Quantity
import RemoteData
import Result.Extra
import Set
import Table
import Theme
import Time exposing (Weekday(..))
import Timetable
import Types exposing (Model, Msg(..), Timetable)
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
                            (\tripStopTimes ->
                                tripStopTimes
                                    |> List.filterMap
                                        (\stopTime ->
                                            Maybe.map4
                                                (\stop_id departure_time arrival_time trip ->
                                                    { stop_id = stop_id
                                                    , departure_time = departure_time
                                                    , arrival_time = arrival_time
                                                    , trip_id = stopTime.trip_id
                                                    , trip_label =
                                                        Maybe.withDefault
                                                            (Id.toString stopTime.trip_id)
                                                            trip.short_name
                                                    , train =
                                                        case trip.block_id of
                                                            Nothing ->
                                                                Id.toString trip.id

                                                            Just block_id ->
                                                                Id.toString trip.route_id
                                                                    ++ " - "
                                                                    ++ Id.toString block_id
                                                    }
                                                )
                                                stopTime.stop_id
                                                stopTime.departure_time
                                                stopTime.arrival_time
                                                (IdDict.get stopTime.trip_id trips)
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
                                                          , label = stopTime.trip_label
                                                          , train = stopTime.train
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
                                            (\{ departure, label, train, arrival } ->
                                                { from = departure
                                                , label = label
                                                , train = train
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
    ( { today = Date.fromCalendarDate 2024 Time.Jul 9
      , timetable = []
      , stops = RemoteData.Loading
      , pathways = RemoteData.Loading
      , stopTimes = RemoteData.Loading
      , calendars = RemoteData.Loading
      , trips = RemoteData.Loading
      , calendarDates = RemoteData.Loading
      , from = Id.fromString "Pde:09162:100"
      , to = Id.fromString "Pit:22095:7049"
      }
    , loadData
    )


loadData : Cmd Msg
loadData =
    [ -- "de" ,
      "oebb-2024"
    , "micotra-2024"
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
            toMsg feed
                (case got of
                    Ok res ->
                        case Csv.Decode.decodeCsv Csv.Decode.FieldNamesFromFirstRow decoder res of
                            Ok val ->
                                Ok val

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
                                Err (Http.BadBody msg)

                    Err e ->
                        Err e
                )
        )


view : Model -> Element Msg
view model =
    Theme.column []
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
        , Theme.column
            [ Ui.border 1
            , Ui.paddingWith { left = Theme.rhythm, bottom = Theme.rhythm, right = Theme.rhythm, top = 0 }
            ]
          <|
            let
                toResult label data =
                    case data of
                        RemoteData.Error e ->
                            Err [ Ui.text (Debug.toString e) ]

                        RemoteData.NotAsked ->
                            Err [ Ui.text (label ++ " not asked") ]

                        RemoteData.Loading ->
                            Err [ Ui.text (label ++ " loading...") ]

                        RemoteData.Loaded loaded ->
                            Ok loaded
            in
            Ok
                (\stops pathways stopTimes trips calendars calendarDates ->
                    pathways
                        |> mergePair stops
                        |> mergePair calendars
                        |> mergePair trips
                        |> mergePair stopTimes
                        |> mergePair calendarDates
                        |> Dict.toList
                        |> List.map (viewFeed model.today)
                )
                |> Result.Extra.andMap (toResult "Stops" model.stops)
                |> Result.Extra.andMap (toResult "Pathways" model.pathways)
                |> Result.Extra.andMap (toResult "Stop times" model.stopTimes)
                |> Result.Extra.andMap (toResult "Trips" model.trips)
                |> Result.Extra.andMap (toResult "Calendars" model.calendars)
                |> Result.Extra.andMap (toResult "Calendar dates" model.calendarDates)
                |> Result.Extra.merge
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
    -> Element msg
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
    Theme.column []
        [ Ui.text feed

        -- , pathfinder stops pathways
        , viewStops stops filteredStops
        , if False then
            viewPathways stops filteredPathways

          else
            Ui.none
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
                |> Theme.column []

          else
            Ui.none
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
            , maybeColumn "wheelchair_boarding" .wheelchair_boarding Table.debug
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
            , Theme.tableColumn "mode" .mode Table.debug
            , Theme.tableColumn "is_bidirectional" .is_bidirectional Table.debug
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
            , maybeColumn "pickup_type" .pickup_type Table.debug
            , maybeColumn "drop_off_type" .drop_off_type Table.debug
            , maybeColumn "continuous_pickup" .continuous_pickup Table.debug
            , maybeColumn "continuous_drop_off" .continuous_drop_off Table.debug
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
