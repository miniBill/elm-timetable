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
import Id exposing (AgencyId, AreaId, BlockId, BookingRuleId, Id, LevelId, LocationGroupId, LocationId, NetworkId, PathwayId, RouteId, ServiceId, ShapeId, StopId, TripId, ZoneId)
import Length exposing (Length)
import List.Extra
import SQLite.Codec as Codec exposing (Codec)
import SQLite.Column as Column exposing (Color, Column, NullableColumn)
import SQLite.Statement as Statement
import SQLite.Statement.CreateTable as CreateTable
import SQLite.Table as Table exposing (Table)
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
    , pickup_booking_rule_id : Maybe (Id BookingRuleId)
    , drop_off_booking_rule_id : Maybe (Id BookingRuleId)
    }


type alias StopTimeColumns =
    { trip_id : Column StopTime (Id TripId)
    , arrival_time : NullableColumn StopTime Clock
    , departure_time : NullableColumn StopTime Clock
    , stop_id : NullableColumn StopTime (Id StopId)
    , location_group_id : NullableColumn StopTime (Id LocationGroupId)
    , location_id : NullableColumn StopTime (Id LocationId)
    , stop_sequence : Column StopTime Int
    , stop_headsign : NullableColumn StopTime String
    , start_pickup_drop_off_window : NullableColumn StopTime Clock
    , end_pickup_drop_off_window : NullableColumn StopTime Clock
    , pickup_type : NullableColumn StopTime PickupDropOffType
    , drop_off_type : NullableColumn StopTime PickupDropOffType
    , continuous_pickup : NullableColumn StopTime PickupDropOffType
    , continuous_drop_off : NullableColumn StopTime PickupDropOffType
    , shape_dist_traveled : NullableColumn StopTime Float

    -- False for approximate, True or Nothing for exact
    , timepoint : NullableColumn StopTime Bool
    , pickup_booking_rule_id : NullableColumn StopTime (Id BookingRuleId)
    , drop_off_booking_rule_id : NullableColumn StopTime (Id BookingRuleId)
    }


stopTimes : Table StopTime StopTimeColumns
stopTimes =
    Table.table "stop_times.txt" "stop_times" StopTime StopTimeColumns
        |> Table.with (Column.column "trip_id" .trip_id Codec.id |> Column.withForeignKey trips .id)
        |> Table.with (Column.nullable "arrival_time" .arrival_time Codec.clock)
        |> Table.with (Column.nullable "departure_time" .departure_time Codec.clock)
        |> Table.with (Column.nullable "stop_id" .stop_id Codec.id |> Column.withForeignKey stops .id)
        |> Table.with (Column.nullable "location_group_id" .location_group_id Codec.id |> Column.withForeignKey locationGroups .id)
        |> Table.with (Column.nullable "location_id" .location_id Codec.id {- |> Column.withForeignKey locations -})
        |> Table.with (Column.column "stop_sequence" .stop_sequence Codec.int)
        |> Table.with (Column.nullable "stop_headsign" .stop_headsign Codec.string)
        |> Table.with (Column.nullable "start_pickup_drop_off_window" .start_pickup_drop_off_window Codec.clock)
        |> Table.with (Column.nullable "end_pickup_drop_off_window" .end_pickup_drop_off_window Codec.clock)
        |> Table.with (Column.nullable "pickup_type" .pickup_type pickupDropOffType)
        |> Table.with (Column.nullable "drop_off_type" .drop_off_type pickupDropOffType)
        |> Table.with (Column.nullable "continuous_pickup" .continuous_pickup pickupDropOffType)
        |> Table.with (Column.nullable "continuous_drop_off" .continuous_drop_off pickupDropOffType)
        |> Table.with (Column.nullable "shape_dist_traveled" .shape_dist_traveled Codec.float)
        |> Table.with (Column.nullable "timepoint" .timepoint Codec.bool)
        |> Table.with (Column.nullable "pickup_booking_rule_id" .pickup_booking_rule_id Codec.id {- |> Column.withForeignKey booking_rules -})
        |> Table.with (Column.nullable "drop_off_booking_rule_id" .drop_off_booking_rule_id Codec.id {- |> Column.withForeignKey booking_rules -})
        |> Table.withPrimaryKey [ "trip_id", "stop_sequence" ]


type alias LocationGroup =
    { id : Id LocationGroupId
    , group_name : Maybe String
    }


type alias LocationGroupColumns =
    { id : Column LocationGroup (Id LocationGroupId)
    , group_name : NullableColumn LocationGroup String
    }


locationGroups : Table LocationGroup LocationGroupColumns
locationGroups =
    Table.table "location_groups.txt" "location_groups" LocationGroup LocationGroupColumns
        |> Table.with (Column.column "location_group_id" .id Codec.id)
        |> Table.with (Column.nullable "location_group_name" .group_name Codec.string)
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


type alias StopColumns =
    { id : Column Stop (Id StopId)
    , code : NullableColumn Stop String
    , name : NullableColumn Stop String
    , tts_name : NullableColumn Stop String
    , description : NullableColumn Stop String
    , lat : NullableColumn Stop Angle
    , lon : NullableColumn Stop Angle
    , zone_id : NullableColumn Stop (Id ZoneId)
    , url : NullableColumn Stop Url
    , location_type : NullableColumn Stop LocationType
    , parent_station : NullableColumn Stop (Id StopId)
    , timezone : NullableColumn Stop Timezone
    , wheelchair_boarding : NullableColumn Stop Accessibility
    , level_id : NullableColumn Stop (Id LevelId)
    , platform_code : NullableColumn Stop String
    }


stops : Table Stop StopColumns
stops =
    let
        name : String
        name =
            "stops"
    in
    Table.table "stops.txt" name Stop StopColumns
        |> Table.with (Column.column "stop_id" .id Codec.id)
        |> Table.with (Column.nullable "stop_code" .code Codec.string)
        |> Table.with (Column.nullable "stop_name" .name Codec.string)
        |> Table.with (Column.nullable "tts_stop_name" .tts_name Codec.string)
        |> Table.with (Column.nullable "stop_desc" .description Codec.string)
        |> Table.with (Column.nullable "stop_lat" .lat Codec.angle)
        |> Table.with (Column.nullable "stop_lon" .lon Codec.angle)
        |> Table.with (Column.nullable "zone_id" .zone_id Codec.id)
        |> Table.with (Column.nullable "stop_url" .url Codec.url)
        |> Table.with (Column.nullable "location_type" .location_type locationType)
        |> Table.with (Column.nullable "parent_station" .parent_station Codec.id |> Column.withSelfForeignKey name "stop_id")
        |> Table.with (Column.nullable "stop_timezone" .timezone Codec.string)
        |> Table.with (Column.nullable "wheelchair_boarding" .wheelchair_boarding accessibility)
        |> Table.with (Column.nullable "level_id" .level_id Codec.id |> Column.withForeignKey levels .id)
        |> Table.with (Column.nullable "platform_code" .platform_code Codec.string)
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


type alias RouteColumns =
    { id : Column Route (Id RouteId)
    , agency_id : NullableColumn Route (Id AgencyId)
    , short_name : NullableColumn Route String
    , long_name : NullableColumn Route String
    , description : NullableColumn Route String
    , tipe : Column Route RouteType
    , url : NullableColumn Route String
    , color : NullableColumn Route Color
    , text_color : NullableColumn Route Color
    , sort_order : NullableColumn Route Int
    , continuous_pickup : NullableColumn Route PickupDropOffType
    , continuous_drop_off : NullableColumn Route PickupDropOffType
    , network_id : NullableColumn Route (Id NetworkId)
    }


routes : Table Route RouteColumns
routes =
    Table.table "routes.txt" "routes" Route RouteColumns
        |> Table.with (Column.column "route_id" .id Codec.id)
        |> Table.with (Column.nullable "agency_id" .agency_id Codec.id |> Column.withForeignKey agency .id)
        |> Table.with (Column.nullable "route_short_name" .short_name Codec.string)
        |> Table.with (Column.nullable "route_long_name" .long_name Codec.string)
        |> Table.with (Column.nullable "route_desc" .description Codec.string)
        |> Table.with (Column.column "route_type" .tipe routeType)
        |> Table.with (Column.nullable "route_url" .url Codec.string)
        |> Table.with (Column.nullable "route_color" .color Column.color)
        |> Table.with (Column.nullable "route_text_color" .text_color Column.color)
        |> Table.with (Column.nullable "route_sort_order" .sort_order Codec.int)
        |> Table.with (Column.nullable "continuous_pickup" .continuous_pickup pickupDropOffType)
        |> Table.with (Column.nullable "continuous_drop_off" .continuous_drop_off pickupDropOffType)
        |> Table.with (Column.nullable "network_id" .network_id Codec.id)
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


type alias TripColumns =
    { route_id : Column Trip (Id RouteId)
    , service_id : Column Trip (Id ServiceId)
    , id : Column Trip (Id TripId)
    , headsign : NullableColumn Trip String
    , short_name : NullableColumn Trip String
    , direction_id : NullableColumn Trip Bool
    , block_id : NullableColumn Trip (Id BlockId)
    , shape_id : NullableColumn Trip (Id ShapeId)
    , wheelchair_accessible : NullableColumn Trip Accessibility
    , bikes_allowed : NullableColumn Trip Accessibility
    }


trips : Table Trip TripColumns
trips =
    Table.table "trips.txt" "trips" Trip TripColumns
        |> Table.with (Column.column "route_id" .route_id Codec.id |> Column.withForeignKey routes .id)
        |> Table.with (Column.column "service_id" .service_id Codec.id {- |> Column.withForeignKey calendars/calendarDate -})
        |> Table.with (Column.column "trip_id" .id Codec.id)
        |> Table.with (Column.nullable "trip_headsign" .headsign Codec.string)
        |> Table.with (Column.nullable "trip_short_name" .short_name Codec.string)
        |> Table.with (Column.nullable "direction_id" .direction_id Codec.bool)
        |> Table.with (Column.nullable "block_id" .block_id Codec.id)
        |> Table.with (Column.nullable "shape_id" .shape_id Codec.id {- |> Column.withForeignKey shapePoints -})
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


type alias ShapePointColumns =
    { shape_id : Column ShapePoint (Id ShapeId)
    , latitude : Column ShapePoint Angle
    , longitude : Column ShapePoint Angle
    , sequence : Column ShapePoint Int
    , distance_traveled : NullableColumn ShapePoint Length
    }


shapePoints : Table ShapePoint ShapePointColumns
shapePoints =
    Table.table "shapes.txt" "shape_points" ShapePoint ShapePointColumns
        |> Table.with (Column.column "shape_id" .shape_id Codec.id)
        |> Table.with (Column.column "shape_pt_lat" .latitude Codec.angle)
        |> Table.with (Column.column "shape_pt_lon" .longitude Codec.angle)
        |> Table.with (Column.column "shape_pt_sequence" .sequence Codec.int)
        |> Table.with (Column.nullable "shape_dist_traveled" .distance_traveled Codec.kilometers)
        |> Table.withPrimaryKey [ "shape_id", "shape_pt_sequence" ]


type alias Frequency =
    { trip_id : Id TripId
    , start_time : Clock
    , end_time : Clock
    , headway : Duration
    , exact_times : Maybe Bool
    }


type alias FrequencyColumns =
    { trip_id : Column Frequency (Id TripId)
    , start_time : Column Frequency Clock
    , end_time : Column Frequency Clock
    , headway : Column Frequency Duration
    , exact_times : NullableColumn Frequency Bool
    }


frequencies : Table Frequency FrequencyColumns
frequencies =
    Table.table "frequencies.txt" "frequencies" Frequency FrequencyColumns
        |> Table.with (Column.column "trip_id" .trip_id Codec.id |> Column.withForeignKey trips .id)
        |> Table.with (Column.column "start_time" .start_time Codec.clock)
        |> Table.with (Column.column "end_time" .end_time Codec.clock)
        |> Table.with (Column.column "headway" .headway Codec.seconds)
        |> Table.with (Column.nullable "exact_times" .exact_times Codec.bool)
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


type alias AgencyColumns =
    { id : Column Agency (Id AgencyId)
    , name : Column Agency String
    , url : Column Agency Url
    , timezone : Column Agency Timezone
    , lang : NullableColumn Agency String
    , phone : NullableColumn Agency String
    , fare_url : NullableColumn Agency String
    , email : NullableColumn Agency String
    }


agency : Table Agency AgencyColumns
agency =
    Table.table "agency.txt" "agencies" Agency AgencyColumns
        |> Table.with (Column.column "agency_id" .id Codec.id)
        |> Table.with (Column.column "agency_name" .name Codec.string)
        |> Table.with (Column.column "agency_url" .url Codec.url)
        |> Table.with (Column.column "agency_timezone" .timezone Codec.string)
        |> Table.with (Column.nullable "agency_lang" .lang Codec.string)
        |> Table.with (Column.nullable "agency_phone" .phone Codec.string)
        |> Table.with (Column.nullable "agency_fare_url" .fare_url Codec.string)
        |> Table.with (Column.nullable "agency_email" .email Codec.string)
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


type alias CalendarColumns =
    { id : Column Calendar (Id ServiceId)
    , monday : Column Calendar Bool
    , tuesday : Column Calendar Bool
    , wednesday : Column Calendar Bool
    , thursday : Column Calendar Bool
    , friday : Column Calendar Bool
    , saturday : Column Calendar Bool
    , sunday : Column Calendar Bool
    , start_date : Column Calendar Date
    , end_date : Column Calendar Date
    }


calendars : Table Calendar CalendarColumns
calendars =
    Table.table "calendar.txt" "calendars" Calendar CalendarColumns
        |> Table.with (Column.column "service_id" .id Codec.id)
        |> Table.with (Column.column "monday" .monday Codec.bool)
        |> Table.with (Column.column "tuesday" .tuesday Codec.bool)
        |> Table.with (Column.column "wednesday" .wednesday Codec.bool)
        |> Table.with (Column.column "thursday" .thursday Codec.bool)
        |> Table.with (Column.column "friday" .friday Codec.bool)
        |> Table.with (Column.column "saturday" .saturday Codec.bool)
        |> Table.with (Column.column "sunday" .sunday Codec.bool)
        |> Table.with (Column.column "start_date" .start_date Codec.date)
        |> Table.with (Column.column "end_date" .end_date Codec.date)
        |> Table.withPrimaryKey [ "service_id" ]


type alias CalendarDate =
    { service_id : Id ServiceId
    , date : Date
    , exception_type : ExceptionType
    }


type alias CalendarDateColumns =
    { service_id : Column CalendarDate (Id ServiceId)
    , date : Column CalendarDate Date
    , exception_type : Column CalendarDate ExceptionType
    }


calendarDates : Table CalendarDate CalendarDateColumns
calendarDates =
    Table.table "calendar_dates.txt" "calendar_dates" CalendarDate CalendarDateColumns
        |> Table.with (Column.column "service_id" .service_id Codec.id)
        |> Table.with (Column.column "date" .date Codec.date)
        |> Table.with (Column.column "exception_type" .exception_type exceptionType)
        |> Table.withPrimaryKey [ "service_id", "date" ]


type alias Area =
    { id : Id AreaId
    , name : Maybe String
    }


type alias AreaColumns =
    { id : Column Area (Id AreaId)
    , name : NullableColumn Area String
    }


areas : Table Area AreaColumns
areas =
    Table.table "areas.txt" "areas" Area AreaColumns
        |> Table.with (Column.column "area_id" .id Codec.id)
        |> Table.with (Column.nullable "area_name" .name Codec.string)
        |> Table.withPrimaryKey [ "area_id" ]


type alias StopArea =
    { area_id : Id AreaId
    , stop_id : Id StopId
    }


type alias StopAreaColumns =
    { area_id : Column StopArea (Id AreaId)
    , stop_id : Column StopArea (Id StopId)
    }


stopAreas : Table StopArea StopAreaColumns
stopAreas =
    Table.table "stop_areas.txt" "stop_areas" StopArea StopAreaColumns
        |> Table.with (Column.column "area_id" .area_id Codec.id |> Column.withForeignKey areas .id)
        |> Table.with (Column.column "stop_id" .stop_id Codec.id |> Column.withForeignKey stops .id)
        |> Table.withPrimaryKey [ "area_id", "stop_id" ]


type alias Network =
    { id : Id NetworkId
    , name : Maybe String
    }


type alias NetworkColumns =
    { id : Column Network (Id NetworkId)
    , name : NullableColumn Network String
    }


networks : Table Network NetworkColumns
networks =
    Table.table "networks.txt" "networks" Network NetworkColumns
        |> Table.with (Column.column "network_id" .id Codec.id)
        |> Table.with (Column.nullable "network_name" .name Codec.string)
        |> Table.withPrimaryKey [ "network_id" ]


type alias RouteNetwork =
    { network_id : Id NetworkId
    , route_id : Id RouteId
    }


type alias RouteNetworkColumns =
    { network_id : Column RouteNetwork (Id NetworkId)
    , route_id : Column RouteNetwork (Id RouteId)
    }


routeNetworks : Table RouteNetwork RouteNetworkColumns
routeNetworks =
    Table.table "route_networks.txt" "route_networks" RouteNetwork RouteNetworkColumns
        |> Table.with (Column.column "network_id" .network_id Codec.id |> Column.withForeignKey networks .id)
        |> Table.with (Column.column "route_id" .route_id Codec.id |> Column.withForeignKey routes .id)
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


type alias PathwayColumns =
    { id : Column Pathway (Id PathwayId)
    , from_stop_id : Column Pathway (Id StopId)
    , to_stop_id : Column Pathway (Id StopId)
    , mode : Column Pathway PathwayMode
    , is_bidirectional : Column Pathway Bool
    , length : NullableColumn Pathway Length
    , traversal_time : NullableColumn Pathway Duration
    , stair_count : NullableColumn Pathway Int
    , max_slope : NullableColumn Pathway Float
    , min_width : NullableColumn Pathway Length
    , signposted_as : NullableColumn Pathway String
    , reversed_signposted_as : NullableColumn Pathway String
    }


pathways : Table Pathway PathwayColumns
pathways =
    Table.table "pathways.txt" "pathways" Pathway PathwayColumns
        |> Table.with (Column.column "pathway_id" .id Codec.id)
        |> Table.with (Column.column "from_stop_id" .from_stop_id Codec.id |> Column.withForeignKey stops .id)
        |> Table.with (Column.column "to_stop_id" .to_stop_id Codec.id |> Column.withForeignKey stops .id)
        |> Table.with (Column.column "pathway_mode" .mode pathwayMode)
        |> Table.with (Column.column "is_bidirectional" .is_bidirectional Codec.bool)
        |> Table.with (Column.nullable "length" .length Codec.meters)
        |> Table.with (Column.nullable "traversal_time" .traversal_time Codec.seconds)
        |> Table.with (Column.nullable "stair_count" .stair_count Codec.int)
        |> Table.with (Column.nullable "max_slope" .max_slope Codec.float)
        |> Table.with (Column.nullable "min_width" .min_width Codec.meters)
        |> Table.with (Column.nullable "signposted_as" .signposted_as Codec.string)
        |> Table.with (Column.nullable "reversed_signposted_as" .reversed_signposted_as Codec.string)
        |> Table.withPrimaryKey [ "pathway_id" ]


type alias Level =
    { id : Id LevelId
    , index : Float
    , name : Maybe String
    }


type alias LevelColumns =
    { id : Column Level (Id LevelId)
    , index : Column Level Float
    , name : NullableColumn Level String
    }


levels : Table Level LevelColumns
levels =
    Table.table "levels.txt" "levels" Level LevelColumns
        |> Table.with (Column.column "level_id" .id Codec.id)
        |> Table.with (Column.column "level_index" .index Codec.float)
        |> Table.with (Column.nullable "level_name" .name Codec.string)
        |> Table.withPrimaryKey [ "level_id" ]



-- Fields --


exceptionType : Codec ExceptionType
exceptionType =
    Codec.andThen parseExceptionType exceptionTypeToInt Codec.int


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
    Codec.andThen parsePickupDropOffType pickupDropOffTypeToInt Codec.int


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
    Codec.andThen parseLocationType locationTypeToInt Codec.int


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
    Codec.andThen parseAccessibility accessibilityToInt Codec.int


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
    Codec.andThen parsePathwayMode pathwayModeToInt Codec.int


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
    Codec.andThen parseRouteType routeTypeToInt Codec.int


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


withFeedColumn : Table a cols -> Table a cols
withFeedColumn t =
    let
        ( columnForeignKeys, columnsWithoutForeignKeys ) =
            t.columnList
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
    { name = t.name
    , filename = t.filename
    , primaryKey = feedColumn.name :: t.primaryKey
    , columnList = feedColumn :: columnsWithoutForeignKeys
    , columns = t.columns
    , encode = t.encode
    , decoder = t.decoder
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
                                feedColumn.name :: colNames

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
