module GTFS exposing (Accessibility(..), Calendar, CalendarDate, ExceptionType(..), Latitude, LocationType(..), Longitude, Pathway, PathwayMode(..), PickupDropOffType(..), Stop, StopTime, Timezone, Trip, dateToInt, locationTypeToString)

import Angle exposing (Angle)
import Clock exposing (Clock)
import Date exposing (Date)
import Duration exposing (Duration)
import Id exposing (BlockId, Id, LevelId, LocationGroupId, LocationId, PathwayId, RouteId, ServiceId, ShapeId, StopId, TripId, ZoneId)
import Length exposing (Length)
import Url exposing (Url)


type alias Timezone =
    String


type alias Latitude =
    Angle


type alias Longitude =
    Angle



-------------------
-- Feed decoders --
-------------------


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


type alias CalendarDate =
    { service_id : Id ServiceId
    , date : Date
    , exception_type : ExceptionType
    }


type ExceptionType
    = ServiceAdded
    | ServiceRemoved


dateToInt : Date -> Int
dateToInt date =
    Date.year date * 10000 + Date.monthNumber date * 100 + Date.day date


type PickupDropOffType
    = RegularlyScheduled
    | NoPickupDropOff
    | PhoneAgency
    | CoordinateWithDriver


type alias Stop =
    { id : Id StopId
    , code : Maybe String
    , name : Maybe String
    , tts_name : Maybe String
    , description : Maybe String
    , lat : Maybe Latitude
    , lon : Maybe Longitude
    , zone_id : Maybe (Id ZoneId)
    , url : Maybe Url
    , location_type : LocationType
    , parent_station : Maybe (Id StopId)
    , timezone : Maybe Timezone
    , wheelchair_boarding : Maybe Accessibility
    , level_id : Maybe (Id LevelId)
    , platform_code : Maybe String
    }


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


type LocationType
    = StopPlatform
    | Station
    | EntranceExit
    | GenericNode
    | BoardingArea


type Accessibility
    = NoAccessibilityInformation
    | Accessibly
    | NotAccessible


type PathwayMode
    = Walkway
    | Stairs
    | MovingSidewalk
    | Escalator
    | Elevator
    | FareGate
    | ExitGate



--------------------
-- Basic decoders --
--------------------


locationTypeToString : LocationType -> String
locationTypeToString locationType =
    case locationType of
        StopPlatform ->
            "Stop / Platform"

        Station ->
            "Station"

        EntranceExit ->
            "Entrance / Exit"

        GenericNode ->
            "Generic node"

        BoardingArea ->
            "Boarding area"
