module Main exposing (main)

import Browser
import Csv.Decode
import Date exposing (Date)
import Dict exposing (Dict)
import Dict.Extra
import GTFS exposing (Calendar, CalendarDate, Feed, Pathway, Stop, StopTime, Trip)
import Html exposing (Html)
import Html.Attributes
import Http
import Id exposing (Id, PathwayId, ServiceId, StopId, TripId)
import IdDict exposing (IdDict)
import IdDict.Extra
import IdSet exposing (IdSet)
import Quantity
import RemoteData
import Set
import Table
import Theme
import Time exposing (Weekday(..))
import Timetable
import Types exposing (Model, Msg(..), Timetable)
import Ui exposing (Element)
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
                                            (\{ departure, label, arrival } ->
                                                { from = departure
                                                , label = label
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
                , Theme.padding
                ]
        , Theme.button []
            { onPress = Reload
            , label = Ui.text "Reload"
            }
        , Theme.column
            [ Ui.border 1
            , Theme.padding
            ]
          <|
            case model.stops of
                RemoteData.Error e ->
                    [ Ui.text (Debug.toString e) ]

                RemoteData.NotAsked ->
                    [ Ui.text "Stops not asked" ]

                RemoteData.Loading ->
                    [ Ui.text "Stops loading..." ]

                RemoteData.Loaded stops ->
                    case model.pathways of
                        RemoteData.Error e ->
                            [ Ui.text (Debug.toString e) ]

                        RemoteData.NotAsked ->
                            [ Ui.text "Pathways not asked" ]

                        RemoteData.Loading ->
                            [ Ui.text "Pathways loading..." ]

                        RemoteData.Loaded pathways ->
                            case model.stopTimes of
                                RemoteData.Error e ->
                                    [ Ui.text (Debug.toString e) ]

                                RemoteData.NotAsked ->
                                    [ Ui.text "Stop times not asked" ]

                                RemoteData.Loading ->
                                    [ Ui.text "Stop times loading..." ]

                                RemoteData.Loaded stopTimes ->
                                    case model.trips of
                                        RemoteData.Error e ->
                                            [ Ui.text (Debug.toString e) ]

                                        RemoteData.NotAsked ->
                                            [ Ui.text "Trips not asked" ]

                                        RemoteData.Loading ->
                                            [ Ui.text "Trips loading..." ]

                                        RemoteData.Loaded trips ->
                                            case model.calendars of
                                                RemoteData.Error e ->
                                                    [ Ui.text (Debug.toString e) ]

                                                RemoteData.NotAsked ->
                                                    [ Ui.text "Calendars not asked" ]

                                                RemoteData.Loading ->
                                                    [ Ui.text "Calendars loading..." ]

                                                RemoteData.Loaded calendars ->
                                                    case model.calendarDates of
                                                        RemoteData.Error e ->
                                                            [ Ui.text (Debug.toString e) ]

                                                        RemoteData.NotAsked ->
                                                            [ Ui.text "Calendar dates not asked" ]

                                                        RemoteData.Loading ->
                                                            [ Ui.text "Calendar dates loading..." ]

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
        |> Ui.html


viewPathways : IdDict StopId Stop -> List Pathway -> Element msg
viewPathways stops filteredPathways =
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

        viewPathway : Pathway -> Html msg
        viewPathway pathway =
            [ Table.id pathway.id
            , Table.string (stopName pathway.from_stop_id)
            , Table.string (stopName pathway.to_stop_id)
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
    Theme.column []
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
            |> Ui.html
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
    filteredStopTimes
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
        |> Ui.html


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
