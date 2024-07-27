module GTFS.Tables exposing
    ( Agency, agency
    , Stop, stops
    , Route, routes
    , Trip, trips
    , StopTime, stopTimes
    , Calendar, calendars
    , CalendarDate, calendarDates
    , ShapePoint, shapePoints
    , Pathway, pathways
    , Level, levels
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
@docs Pathway, pathways
@docs Level, levels

-}

import Angle exposing (Angle)
import Clock exposing (Clock)
import Date exposing (Date)
import Duration exposing (Duration)
import GTFS exposing (Accessibility(..), ExceptionType(..), LocationType(..), PathwayMode(..), PickupDropOffType(..), RouteType(..), Timezone)
import Id exposing (AgencyId, BlockId, Id, LevelId, LocationGroupId, LocationId, NetworkId, PathwayId, RouteId, ServiceId, ShapeId, StopId, TripId, ZoneId)
import Length exposing (Length)
import SQLite.TableBuilder exposing (Codec, Color, Table, andThen, angle, bool, clock, color, column, date, float, id, int, kilometers, meters, nullableColumn, seconds, string, table, url, with, withForeignKey, withForeignKeyTo, withPrimaryKey)
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
    table "stop_times.txt" "stop_times" StopTime
        |> with (column "trip_id" .trip_id id |> withForeignKey trips.name)
        |> with (nullableColumn "arrival_time" .arrival_time clock)
        |> with (nullableColumn "departure_time" .departure_time clock)
        |> with (nullableColumn "stop_id" .stop_id id |> withForeignKey stops.name)
        |> with (nullableColumn "location_group_id" .location_group_id id {- |> withForeignKey location_groups.name -})
        |> with (nullableColumn "location_id" .location_id id {- |> withForeignKey locations.name -})
        |> with (column "stop_sequence" .stop_sequence int)
        |> with (nullableColumn "stop_headsign" .stop_headsign string)
        |> with (nullableColumn "start_pickup_drop_off_window" .start_pickup_drop_off_window clock)
        |> with (nullableColumn "end_pickup_drop_off_window" .end_pickup_drop_off_window clock)
        |> with (nullableColumn "pickup_type" .pickup_type pickupDropOffType)
        |> with (nullableColumn "drop_off_type" .drop_off_type pickupDropOffType)
        |> with (nullableColumn "continuous_pickup" .continuous_pickup pickupDropOffType)
        |> with (nullableColumn "continuous_drop_off" .continuous_drop_off pickupDropOffType)
        |> with (nullableColumn "shape_dist_traveled" .shape_dist_traveled float)
        |> with (nullableColumn "timepoint" .timepoint bool)
        |> with (nullableColumn "pickup_booking_rule_id" .pickup_booking_rule_id id {- |> withForeignKey booking_rules.name -})
        |> with (nullableColumn "drop_off_booking_rule_id" .drop_off_booking_rule_id id {- |> withForeignKey booking_rules.name -})
        |> withPrimaryKey [ "trip_id", "stop_sequence" ]


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
    table "trips.txt" "trips" Trip
        |> with (column "route_id" .route_id id |> withForeignKey routes.name)
        |> with (column "service_id" .service_id id)
        |> with (column "trip_id" .id id)
        |> with (nullableColumn "trip_headsign" .headsign string)
        |> with (nullableColumn "trip_short_name" .short_name string)
        |> with (nullableColumn "direction_id" .direction_id bool)
        |> with (nullableColumn "block_id" .block_id id)
        |> with (nullableColumn "shape_id" .shape_id id {- |> withForeignKey shapePoints.name -})
        |> with (nullableColumn "wheelchair_accessible" .wheelchair_accessible accessibility)
        |> with (nullableColumn "bikes_allowed" .bikes_allowed accessibility)
        |> withPrimaryKey [ "trip_id" ]


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
    table "routes.txt" "routes" Route
        |> with (column "route_id" .id id)
        |> with (nullableColumn "agency_id" .agency_id id |> withForeignKey agency.name)
        |> with (nullableColumn "route_short_name" .short_name string)
        |> with (nullableColumn "route_long_name" .long_name string)
        |> with (nullableColumn "route_desc" .description string)
        |> with (column "route_type" .tipe routeType)
        |> with (nullableColumn "route_url" .url string)
        |> with (nullableColumn "route_color" .color color)
        |> with (nullableColumn "route_text_color" .text_color color)
        |> with (nullableColumn "route_sort_order" .sort_order int)
        |> with (nullableColumn "continuous_pickup" .continuous_pickup pickupDropOffType)
        |> with (nullableColumn "continuous_drop_off" .continuous_drop_off pickupDropOffType)
        |> with (nullableColumn "network_id" .network_id id)
        |> withPrimaryKey [ "route_id" ]


type alias ShapePoint =
    { shape_id : Id ShapeId
    , latitude : Angle
    , longitude : Angle
    , sequence : Int
    , distance_traveled : Maybe Length
    }


shapePoints : Table ShapePoint
shapePoints =
    table "shapes.txt" "shape_points" ShapePoint
        |> with (column "shape_id" .shape_id id)
        |> with (column "shape_pt_lat" .latitude angle)
        |> with (column "shape_pt_lon" .longitude angle)
        |> with (column "shape_pt_sequence" .sequence int)
        |> with (nullableColumn "shape_dist_traveled" .distance_traveled kilometers)
        |> withPrimaryKey [ "shape_id", "shape_pt_sequence" ]


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
    table "agency.txt" "agencies" Agency
        |> with (column "agency_id" .id id)
        |> with (column "agency_name" .name string)
        |> with (column "agency_url" .url url)
        |> with (column "agency_timezone" .timezone string)
        |> with (nullableColumn "agency_lang" .lang string)
        |> with (nullableColumn "agency_phone" .phone string)
        |> with (nullableColumn "agency_fare_url" .fare_url string)
        |> with (nullableColumn "agency_email" .email string)
        |> withPrimaryKey [ "agency_id" ]


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
    table "calendar.txt" "calendars" Calendar
        |> with (column "service_id" .id id)
        |> with (column "monday" .monday bool)
        |> with (column "tuesday" .tuesday bool)
        |> with (column "wednesday" .wednesday bool)
        |> with (column "thursday" .thursday bool)
        |> with (column "friday" .friday bool)
        |> with (column "saturday" .saturday bool)
        |> with (column "sunday" .sunday bool)
        |> with (column "start_date" .start_date date)
        |> with (column "end_date" .end_date date)
        |> withPrimaryKey [ "service_id" ]


type alias CalendarDate =
    { service_id : Id ServiceId
    , date : Date
    , exception_type : ExceptionType
    }


calendarDates : Table CalendarDate
calendarDates =
    table "calendar_dates.txt" "calendar_dates" CalendarDate
        |> with (column "service_id" .service_id id)
        |> with (column "date" .date date)
        |> with (column "exception_type" .exception_type exceptionType)
        |> withPrimaryKey [ "service_id", "date" ]


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
    , location_type : LocationType
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
    table "stops.txt" name Stop
        |> with (column "stop_id" .id id)
        |> with (nullableColumn "stop_code" .code string)
        |> with (nullableColumn "stop_name" .name string)
        |> with (nullableColumn "tts_stop_name" .tts_name string)
        |> with (nullableColumn "stop_desc" .description string)
        |> with (nullableColumn "stop_lat" .lat angle)
        |> with (nullableColumn "stop_lon" .lon angle)
        |> with (nullableColumn "zone_id" .zone_id id)
        |> with (nullableColumn "stop_url" .url url)
        |> with (column "location_type" .location_type locationType)
        |> with (nullableColumn "parent_station" .parent_station id |> withForeignKeyTo name "stop_id")
        |> with (nullableColumn "stop_timezone" .timezone string)
        |> with (nullableColumn "wheelchair_boarding" .wheelchair_boarding accessibility)
        |> with (nullableColumn "level_id" .level_id id |> withForeignKey levels.name)
        |> with (nullableColumn "platform_code" .platform_code string)
        |> withPrimaryKey [ "stop_id" ]


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
    table "pathways.txt" "pathways" Pathway
        |> with (column "pathway_id" .id id)
        |> with (column "from_stop_id" .from_stop_id id |> withForeignKeyTo stops.name "stop_id")
        |> with (column "to_stop_id" .to_stop_id id |> withForeignKeyTo stops.name "stop_id")
        |> with (column "pathway_mode" .mode pathwayMode)
        |> with (column "is_bidirectional" .is_bidirectional bool)
        |> with (nullableColumn "length" .length meters)
        |> with (nullableColumn "traversal_time" .traversal_time seconds)
        |> with (nullableColumn "stair_count" .stair_count int)
        |> with (nullableColumn "max_slope" .max_slope float)
        |> with (nullableColumn "min_width" .min_width meters)
        |> with (nullableColumn "signposted_as" .signposted_as string)
        |> with (nullableColumn "reversed_signposted_as" .reversed_signposted_as string)
        |> withPrimaryKey [ "pathway_id" ]


type alias Level =
    { id : Id LevelId
    , index : Float
    , name : Maybe String
    }


levels : Table Level
levels =
    table "levels.txt" "levels" Level
        |> with (column "level_id" .id id)
        |> with (column "level_index" .index float)
        |> with (nullableColumn "level_name" .name string)
        |> withPrimaryKey [ "level_id" ]



-- Fields --


exceptionType : Codec ExceptionType
exceptionType =
    andThen parseExceptionType exceptionTypeToInt int


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
    andThen parsePickupDropOffType pickupDropOffTypeToInt int


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
    andThen parseLocationType locationTypeToInt int


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
    andThen parseAccessibility accessibilityToInt int


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
    andThen parsePathwayMode pathwayModeToInt int


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
    andThen parseRouteType routeTypeToInt int


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
