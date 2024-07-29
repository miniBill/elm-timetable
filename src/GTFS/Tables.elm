module GTFS.Tables exposing
    ( Agency, agency
    , Stop, stops
    , Route, routes
    , Trip, trips
    , StopTime, stopTimes
    , Calendar, calendars
    , CalendarDate, calendarDates
    , ShapePoint, shapePoints
    , Frequency, frequencies
    , Pathway, pathways
    , Level, levels
    , Area, areas
    , StopArea, stopAreas
    , LocationGroup, locationGroups
    , Network, networks
    , RouteNetwork, routeNetworks
    , allCreates
    )

{-|

@docs Agency, agency
@docs Stop, stops
@docs Route, routes
@docs Trip, trips
@docs StopTime, stopTimes
@docs Calendar, calendars
@docs CalendarDate, calendarDates
@docs ShapePoint, shapePoints
@docs Frequency, frequencies
@docs Pathway, pathways
@docs Level, levels
@docs Area, areas
@docs StopArea, stopAreas
@docs LocationGroup, locationGroups
@docs Network, networks
@docs RouteNetwork, routeNetworks

@docs allCreates

-}

import Angle exposing (Angle)
import Clock exposing (Clock)
import Date exposing (Date)
import Duration exposing (Duration)
import GTFS exposing (Accessibility(..), ExceptionType(..), LocationType(..), PathwayMode(..), PickupDropOffType(..), RouteType(..), Timezone)
import GTFS.ToSQL
import Id exposing (AgencyId, AreaId, BlockId, Id, LevelId, LocationGroupId, LocationId, NetworkId, PathwayId, RouteId, ServiceId, ShapeId, StopAreaId, StopId, TripId, ZoneId)
import Length exposing (Length)
import List.Extra
import SQLite.Column as Column exposing (Color)
import SQLite.Statement as Statement
import SQLite.Statement.CreateTable as CreateTable
import SQLite.Table as Table exposing (Codec, Table)
import SQLite.Types
import Url exposing (Url)



-- Tables --


type alias StopTime =
    { trip_id : Id TripId
    , arrival_time : Maybe Clock
    , departure_time : Maybe Clock
    , stop_id : Maybe (Id StopId)
    , location_group_id : Maybe (Id LocationGroupId)
    , location_id : Maybe (Id LocationId)
    , stop_sequence : Int
    , stop_headsign : Maybe String
    , start_pickup_drop_off_window : Maybe Clock
    , end_pickup_drop_off_window : Maybe Clock
    , pickup_type : Maybe PickupDropOffType
    , drop_off_type : Maybe PickupDropOffType
    , continuous_pickup : Maybe PickupDropOffType
    , continuous_drop_off : Maybe PickupDropOffType
    , shape_dist_traveled : Maybe Float

    -- False for approximate, True or Nothing for exact
    , timepoint : Maybe Bool
    , pickup_booking_rule_id : Maybe (Id Never)
    , drop_off_booking_rule_id : Maybe (Id Never)
    }


stopTimes : Table StopTime
stopTimes =
    Table.table "stop_times.txt" "stop_times" StopTime
        |> Table.with (Column.notNull "trip_id" .trip_id Column.id |> Column.withForeignKey trips)
        |> Table.with (Column.nullable "arrival_time" .arrival_time Column.clock)
        |> Table.with (Column.nullable "departure_time" .departure_time Column.clock)
        |> Table.with (Column.nullable "stop_id" .stop_id Column.id |> Column.withForeignKey stops)
        |> Table.with (Column.nullable "location_group_id" .location_group_id Column.id |> Column.withForeignKey locationGroups)
        |> Table.with (Column.nullable "location_id" .location_id Column.id {- |> Column.withForeignKey locations -})
        |> Table.with (Column.notNull "stop_sequence" .stop_sequence Column.int)
        |> Table.with (Column.nullable "stop_headsign" .stop_headsign Column.string)
        |> Table.with (Column.nullable "start_pickup_drop_off_window" .start_pickup_drop_off_window Column.clock)
        |> Table.with (Column.nullable "end_pickup_drop_off_window" .end_pickup_drop_off_window Column.clock)
        |> Table.with (Column.nullable "pickup_type" .pickup_type pickupDropOffType)
        |> Table.with (Column.nullable "drop_off_type" .drop_off_type pickupDropOffType)
        |> Table.with (Column.nullable "continuous_pickup" .continuous_pickup pickupDropOffType)
        |> Table.with (Column.nullable "continuous_drop_off" .continuous_drop_off pickupDropOffType)
        |> Table.with (Column.nullable "shape_dist_traveled" .shape_dist_traveled Column.float)
        |> Table.with (Column.nullable "timepoint" .timepoint Column.bool)
        |> Table.with (Column.nullable "pickup_booking_rule_id" .pickup_booking_rule_id Column.id {- |> Column.withForeignKey booking_rules -})
        |> Table.with (Column.nullable "drop_off_booking_rule_id" .drop_off_booking_rule_id Column.id {- |> Column.withForeignKey booking_rules -})
        |> Table.withPrimaryKey [ "trip_id", "stop_sequence" ]


type alias LocationGroup =
    { id : Id LocationGroupId
    , group_name : Maybe String
    }


locationGroups : Table LocationGroup
locationGroups =
    Table.table "location_groups.txt" "location_groups" LocationGroup
        |> Table.with (Column.notNull "location_group_id" .id Column.id)
        |> Table.with (Column.nullable "location_group_name" .group_name Column.string)
        |> Table.withPrimaryKey [ "location_group_id" ]


type alias Stop =
    { id : Id StopId
    , code : Maybe String
    , name : Maybe String
    , tts_name : Maybe String
    , description : Maybe String
    , lat : Maybe Angle
    , lon : Maybe Angle
    , zone_id : Maybe (Id ZoneId)
    , url : Maybe Url
    , location_type : Maybe LocationType
    , parent_station : Maybe (Id StopId)
    , timezone : Maybe Timezone
    , wheelchair_boarding : Maybe Accessibility
    , level_id : Maybe (Id LevelId)
    , platform_code : Maybe String
    }


stops : Table Stop
stops =
    let
        name : String
        name =
            "stops"
    in
    Table.table "stops.txt" name Stop
        |> Table.with (Column.notNull "stop_id" .id Column.id)
        |> Table.with (Column.nullable "stop_code" .code Column.string)
        |> Table.with (Column.nullable "stop_name" .name Column.string)
        |> Table.with (Column.nullable "tts_stop_name" .tts_name Column.string)
        |> Table.with (Column.nullable "stop_desc" .description Column.string)
        |> Table.with (Column.nullable "stop_lat" .lat Column.angle)
        |> Table.with (Column.nullable "stop_lon" .lon Column.angle)
        |> Table.with (Column.nullable "zone_id" .zone_id Column.id)
        |> Table.with (Column.nullable "stop_url" .url Column.url)
        |> Table.with (Column.nullable "location_type" .location_type locationType)
        |> Table.with (Column.nullable "parent_station" .parent_station Column.id |> Column.withForeignKeyTo { name = name } "stop_id")
        |> Table.with (Column.nullable "stop_timezone" .timezone Column.string)
        |> Table.with (Column.nullable "wheelchair_boarding" .wheelchair_boarding accessibility)
        |> Table.with (Column.nullable "level_id" .level_id Column.id |> Column.withForeignKey levels)
        |> Table.with (Column.nullable "platform_code" .platform_code Column.string)
        |> Table.withPrimaryKey [ "stop_id" ]


type alias Route =
    { id : Id RouteId
    , agency_id : Maybe (Id AgencyId)
    , short_name : Maybe String
    , long_name : Maybe String
    , description : Maybe String
    , tipe : RouteType
    , url : Maybe String
    , color : Maybe Color
    , text_color : Maybe Color
    , sort_order : Maybe Int
    , continuous_pickup : Maybe PickupDropOffType
    , continuous_drop_off : Maybe PickupDropOffType
    , network_id : Maybe (Id NetworkId)
    }


routes : Table Route
routes =
    Table.table "routes.txt" "routes" Route
        |> Table.with (Column.notNull "route_id" .id Column.id)
        |> Table.with (Column.nullable "agency_id" .agency_id Column.id |> Column.withForeignKey agency)
        |> Table.with (Column.nullable "route_short_name" .short_name Column.string)
        |> Table.with (Column.nullable "route_long_name" .long_name Column.string)
        |> Table.with (Column.nullable "route_desc" .description Column.string)
        |> Table.with (Column.notNull "route_type" .tipe routeType)
        |> Table.with (Column.nullable "route_url" .url Column.string)
        |> Table.with (Column.nullable "route_color" .color Column.color)
        |> Table.with (Column.nullable "route_text_color" .text_color Column.color)
        |> Table.with (Column.nullable "route_sort_order" .sort_order Column.int)
        |> Table.with (Column.nullable "continuous_pickup" .continuous_pickup pickupDropOffType)
        |> Table.with (Column.nullable "continuous_drop_off" .continuous_drop_off pickupDropOffType)
        |> Table.with (Column.nullable "network_id" .network_id Column.id)
        |> Table.withPrimaryKey [ "route_id" ]


type alias Trip =
    { route_id : Id RouteId
    , service_id : Id ServiceId
    , id : Id TripId
    , headsign : Maybe String
    , short_name : Maybe String
    , direction_id : Maybe Bool
    , block_id : Maybe (Id BlockId)
    , shape_id : Maybe (Id ShapeId)
    , wheelchair_accessible : Maybe Accessibility
    , bikes_allowed : Maybe Accessibility
    }


trips : Table Trip
trips =
    Table.table "trips.txt" "trips" Trip
        |> Table.with (Column.notNull "route_id" .route_id Column.id |> Column.withForeignKey routes)
        |> Table.with (Column.notNull "service_id" .service_id Column.id {- |> Column.withForeignKey calendars/calendarDate -})
        |> Table.with (Column.notNull "trip_id" .id Column.id)
        |> Table.with (Column.nullable "trip_headsign" .headsign Column.string)
        |> Table.with (Column.nullable "trip_short_name" .short_name Column.string)
        |> Table.with (Column.nullable "direction_id" .direction_id Column.bool)
        |> Table.with (Column.nullable "block_id" .block_id Column.id)
        |> Table.with (Column.nullable "shape_id" .shape_id Column.id {- |> Column.withForeignKey shapePoints -})
        |> Table.with (Column.nullable "wheelchair_accessible" .wheelchair_accessible accessibility)
        |> Table.with (Column.nullable "bikes_allowed" .bikes_allowed accessibility)
        |> Table.withPrimaryKey [ "trip_id" ]


type alias ShapePoint =
    { shape_id : Id ShapeId
    , latitude : Angle
    , longitude : Angle
    , sequence : Int
    , distance_traveled : Maybe Length
    }


shapePoints : Table ShapePoint
shapePoints =
    Table.table "shapes.txt" "shape_points" ShapePoint
        |> Table.with (Column.notNull "shape_id" .shape_id Column.id)
        |> Table.with (Column.notNull "shape_pt_lat" .latitude Column.angle)
        |> Table.with (Column.notNull "shape_pt_lon" .longitude Column.angle)
        |> Table.with (Column.notNull "shape_pt_sequence" .sequence Column.int)
        |> Table.with (Column.nullable "shape_dist_traveled" .distance_traveled Column.kilometers)
        |> Table.withPrimaryKey [ "shape_id", "shape_pt_sequence" ]


type alias Frequency =
    { trip_id : Id TripId
    , start_time : Clock
    , end_time : Clock
    , headway : Duration
    , exact_times : Maybe Bool
    }


frequencies : Table Frequency
frequencies =
    Table.table "frequencies.txt" "frequencies" Frequency
        |> Table.with (Column.notNull "trip_id" .trip_id Column.id |> Column.withForeignKey trips)
        |> Table.with (Column.notNull "start_time" .start_time Column.clock)
        |> Table.with (Column.notNull "end_time" .end_time Column.clock)
        |> Table.with (Column.notNull "headway" .headway Column.seconds)
        |> Table.with (Column.nullable "exact_times" .exact_times Column.bool)
        |> Table.withPrimaryKey [ "trip_id", "start_time" ]


type alias Agency =
    { id : Id AgencyId
    , name : String
    , url : Url
    , timezone : Timezone
    , lang : Maybe String
    , phone : Maybe String
    , fare_url : Maybe String
    , email : Maybe String
    }


agency : Table Agency
agency =
    Table.table "agency.txt" "agencies" Agency
        |> Table.with (Column.notNull "agency_id" .id Column.id)
        |> Table.with (Column.notNull "agency_name" .name Column.string)
        |> Table.with (Column.notNull "agency_url" .url Column.url)
        |> Table.with (Column.notNull "agency_timezone" .timezone Column.string)
        |> Table.with (Column.nullable "agency_lang" .lang Column.string)
        |> Table.with (Column.nullable "agency_phone" .phone Column.string)
        |> Table.with (Column.nullable "agency_fare_url" .fare_url Column.string)
        |> Table.with (Column.nullable "agency_email" .email Column.string)
        |> Table.withPrimaryKey [ "agency_id" ]


type alias Calendar =
    { id : Id ServiceId
    , monday : Bool
    , tuesday : Bool
    , wednesday : Bool
    , thursday : Bool
    , friday : Bool
    , saturday : Bool
    , sunday : Bool
    , start_date : Date
    , end_date : Date
    }


calendars : Table Calendar
calendars =
    Table.table "calendar.txt" "calendars" Calendar
        |> Table.with (Column.notNull "service_id" .id Column.id)
        |> Table.with (Column.notNull "monday" .monday Column.bool)
        |> Table.with (Column.notNull "tuesday" .tuesday Column.bool)
        |> Table.with (Column.notNull "wednesday" .wednesday Column.bool)
        |> Table.with (Column.notNull "thursday" .thursday Column.bool)
        |> Table.with (Column.notNull "friday" .friday Column.bool)
        |> Table.with (Column.notNull "saturday" .saturday Column.bool)
        |> Table.with (Column.notNull "sunday" .sunday Column.bool)
        |> Table.with (Column.notNull "start_date" .start_date Column.date)
        |> Table.with (Column.notNull "end_date" .end_date Column.date)
        |> Table.withPrimaryKey [ "service_id" ]


type alias CalendarDate =
    { service_id : Id ServiceId
    , date : Date
    , exception_type : ExceptionType
    }


calendarDates : Table CalendarDate
calendarDates =
    Table.table "calendar_dates.txt" "calendar_dates" CalendarDate
        |> Table.with (Column.notNull "service_id" .service_id Column.id)
        |> Table.with (Column.notNull "date" .date Column.date)
        |> Table.with (Column.notNull "exception_type" .exception_type exceptionType)
        |> Table.withPrimaryKey [ "service_id", "date" ]


type alias Area =
    { id : Id AreaId
    , name : Maybe String
    }


areas : Table Area
areas =
    Table.table "areas.txt" "areas" Area
        |> Table.with (Column.notNull "area_id" .id Column.id)
        |> Table.with (Column.nullable "area_name" .name Column.string)
        |> Table.withPrimaryKey [ "area_id" ]


type alias StopArea =
    { area_id : Id StopAreaId
    , stop_id : Id StopId
    }


stopAreas : Table StopArea
stopAreas =
    Table.table "stop_areas.txt" "stop_areas" StopArea
        |> Table.with (Column.notNull "area_id" .area_id Column.id |> Column.withForeignKey areas)
        |> Table.with (Column.notNull "stop_id" .stop_id Column.id |> Column.withForeignKey stops)
        |> Table.withPrimaryKey [ "area_id", "stop_id" ]


type alias Network =
    { id : Id NetworkId
    , name : Maybe String
    }


networks : Table Network
networks =
    Table.table "networks.txt" "networks" Network
        |> Table.with (Column.notNull "network_id" .id Column.id)
        |> Table.with (Column.nullable "network_name" .name Column.string)
        |> Table.withPrimaryKey [ "network_id" ]


type alias RouteNetwork =
    { network_id : Id NetworkId
    , route_id : Id RouteId
    }


routeNetworks : Table RouteNetwork
routeNetworks =
    Table.table "route_networks.txt" "route_networks" RouteNetwork
        |> Table.with (Column.notNull "network_id" .network_id Column.id |> Column.withForeignKey networks)
        |> Table.with (Column.notNull "route_id" .route_id Column.id |> Column.withForeignKey routes)
        |> Table.withPrimaryKey [ "route_id" ]


type alias Pathway =
    { id : Id PathwayId
    , from_stop_id : Id StopId
    , to_stop_id : Id StopId
    , mode : PathwayMode
    , is_bidirectional : Bool
    , length : Maybe Length
    , traversal_time : Maybe Duration
    , stair_count : Maybe Int
    , max_slope : Maybe Float
    , min_width : Maybe Length
    , signposted_as : Maybe String
    , reversed_signposted_as : Maybe String
    }


pathways : Table Pathway
pathways =
    Table.table "pathways.txt" "pathways" Pathway
        |> Table.with (Column.notNull "pathway_id" .id Column.id)
        |> Table.with (Column.notNull "from_stop_id" .from_stop_id Column.id |> Column.withForeignKeyTo stops "stop_id")
        |> Table.with (Column.notNull "to_stop_id" .to_stop_id Column.id |> Column.withForeignKeyTo stops "stop_id")
        |> Table.with (Column.notNull "pathway_mode" .mode pathwayMode)
        |> Table.with (Column.notNull "is_bidirectional" .is_bidirectional Column.bool)
        |> Table.with (Column.nullable "length" .length Column.meters)
        |> Table.with (Column.nullable "traversal_time" .traversal_time Column.seconds)
        |> Table.with (Column.nullable "stair_count" .stair_count Column.int)
        |> Table.with (Column.nullable "max_slope" .max_slope Column.float)
        |> Table.with (Column.nullable "min_width" .min_width Column.meters)
        |> Table.with (Column.nullable "signposted_as" .signposted_as Column.string)
        |> Table.with (Column.nullable "reversed_signposted_as" .reversed_signposted_as Column.string)
        |> Table.withPrimaryKey [ "pathway_id" ]


type alias Level =
    { id : Id LevelId
    , index : Float
    , name : Maybe String
    }


levels : Table Level
levels =
    Table.table "levels.txt" "levels" Level
        |> Table.with (Column.notNull "level_id" .id Column.id)
        |> Table.with (Column.notNull "level_index" .index Column.float)
        |> Table.with (Column.nullable "level_name" .name Column.string)
        |> Table.withPrimaryKey [ "level_id" ]



-- Fields --


exceptionType : Codec ExceptionType
exceptionType =
    Column.andThen parseExceptionType exceptionTypeToInt Column.int


exceptionTypeToInt : ExceptionType -> Int
exceptionTypeToInt input =
    case input of
        ServiceAdded ->
            1

        ServiceRemoved ->
            2


parseExceptionType : Int -> Result String ExceptionType
parseExceptionType input =
    case input of
        1 ->
            Ok ServiceAdded

        2 ->
            Ok ServiceRemoved

        _ ->
            Err (String.fromInt input ++ " is not a valid exception type")


pickupDropOffType : Codec PickupDropOffType
pickupDropOffType =
    Column.andThen parsePickupDropOffType pickupDropOffTypeToInt Column.int


pickupDropOffTypeToInt : PickupDropOffType -> Int
pickupDropOffTypeToInt input =
    case input of
        RegularlyScheduled ->
            0

        NoPickupDropOff ->
            1

        PhoneAgency ->
            2

        CoordinateWithDriver ->
            3


parsePickupDropOffType : Int -> Result String PickupDropOffType
parsePickupDropOffType input =
    case input of
        0 ->
            Ok RegularlyScheduled

        1 ->
            Ok NoPickupDropOff

        2 ->
            Ok PhoneAgency

        3 ->
            Ok CoordinateWithDriver

        _ ->
            Err (String.fromInt input ++ " is not a valid pickup/drop off type")


locationType : Codec LocationType
locationType =
    Column.andThen parseLocationType locationTypeToInt Column.int


locationTypeToInt : LocationType -> Int
locationTypeToInt input =
    case input of
        StopPlatform ->
            0

        Station ->
            1

        EntranceExit ->
            2

        GenericNode ->
            3

        BoardingArea ->
            4


parseLocationType : Int -> Result String LocationType
parseLocationType input =
    case input of
        0 ->
            Ok StopPlatform

        1 ->
            Ok Station

        2 ->
            Ok EntranceExit

        3 ->
            Ok GenericNode

        4 ->
            Ok BoardingArea

        _ ->
            Err (String.fromInt input ++ " is not a valid location type")


accessibility : Codec Accessibility
accessibility =
    Column.andThen parseAccessibility accessibilityToInt Column.int


accessibilityToInt : Accessibility -> Int
accessibilityToInt input =
    case input of
        NoAccessibilityInformation ->
            0

        Accessibly ->
            1

        NotAccessible ->
            2


parseAccessibility : Int -> Result String Accessibility
parseAccessibility input =
    case input of
        0 ->
            Ok NoAccessibilityInformation

        1 ->
            Ok Accessibly

        2 ->
            Ok NotAccessible

        _ ->
            Err (String.fromInt input ++ " is not a valid accessibility")


pathwayMode : Codec PathwayMode
pathwayMode =
    Column.andThen parsePathwayMode pathwayModeToInt Column.int


pathwayModeToInt : PathwayMode -> Int
pathwayModeToInt input =
    case input of
        Walkway ->
            1

        Stairs ->
            2

        MovingSidewalk ->
            3

        Escalator ->
            4

        Elevator ->
            5

        FareGate ->
            6

        ExitGate ->
            7


parsePathwayMode : Int -> Result String PathwayMode
parsePathwayMode input =
    case input of
        1 ->
            Ok Walkway

        2 ->
            Ok Stairs

        3 ->
            Ok MovingSidewalk

        4 ->
            Ok Escalator

        5 ->
            Ok Elevator

        6 ->
            Ok FareGate

        7 ->
            Ok ExitGate

        _ ->
            Err (String.fromInt input ++ " is not a valid pathway mode")


routeType : Codec RouteType
routeType =
    Column.andThen parseRouteType routeTypeToInt Column.int


routeTypeToInt : RouteType -> Int
routeTypeToInt tipe =
    case tipe of
        TramStreetcarLightRail ->
            0

        SubwayMetro ->
            1

        Rail ->
            2

        Bus ->
            3

        Ferry ->
            4

        CableTram ->
            5

        AerialLift ->
            6

        Funicular ->
            7

        Trolleybus ->
            11

        Monorail ->
            12


parseRouteType : Int -> Result String RouteType
parseRouteType input =
    case input of
        0 ->
            Ok TramStreetcarLightRail

        1 ->
            Ok SubwayMetro

        2 ->
            Ok Rail

        3 ->
            Ok Bus

        4 ->
            Ok Ferry

        5 ->
            Ok CableTram

        6 ->
            Ok AerialLift

        7 ->
            Ok Funicular

        11 ->
            Ok Trolleybus

        12 ->
            Ok Monorail

        _ ->
            Err (String.fromInt input ++ " is not a valid route type")


withFeedColumn : Table a -> Table a
withFeedColumn t =
    let
        ( columnForeignKeys, columnsWithoutForeignKeys ) =
            t.columns
                |> List.map
                    (\column ->
                        case
                            List.Extra.findMap
                                (\{ constraint } ->
                                    case constraint of
                                        CreateTable.ColumnForeignKey fk ->
                                            Just fk

                                        _ ->
                                            Nothing
                                )
                                column.constraints
                        of
                            Just fk ->
                                ( Just ( column.name, fk )
                                , { column
                                    | constraints =
                                        List.Extra.removeWhen
                                            (\{ constraint } -> constraint == CreateTable.ColumnForeignKey fk)
                                            column.constraints
                                  }
                                )

                            Nothing ->
                                ( Nothing, column )
                    )
                |> List.unzip
                |> Tuple.mapFirst (List.filterMap identity)
    in
    { t
        | primaryKey = feedColumn.name :: t.primaryKey
        , columns = feedColumn :: columnsWithoutForeignKeys
        , foreignKeys =
            (t.foreignKeys
                ++ List.map
                    (\( colName, fk ) ->
                        ( [ colName ], fk )
                    )
                    columnForeignKeys
            )
                |> List.map
                    (\( colNames, fk ) ->
                        ( feedColumn.name :: colNames
                        , { fk
                            | columnNames =
                                if List.isEmpty fk.columnNames then
                                    []

                                else
                                    feedColumn.name :: fk.columnNames
                          }
                        )
                    )
    }


feedColumn : CreateTable.ColumnDefinition
feedColumn =
    { name = "feed"
    , tipe = Just SQLite.Types.Text
    , constraints =
        [ { name = Nothing, constraint = CreateTable.ColumnNotNull Nothing } ]
    }


allCreates : List Statement.Statement
allCreates =
    [ GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn agency)
    , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn stops)
    , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn routes)
    , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn trips)
    , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn stopTimes)
    , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn calendars)
    , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn calendarDates)
    , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn areas)
    , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn stopAreas)
    , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn networks)
    , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn routeNetworks)
    , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn shapePoints)
    , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn frequencies)

    -- , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn transfers)
    , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn pathways)
    , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn levels)
    , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn locationGroups)

    -- , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn locationGroupStops)
    -- , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn translations)
    -- , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn feedInfo)
    -- , GTFS.ToSQL.toCreate { ifNotExists = False } (withFeedColumn attributions)
    ]
