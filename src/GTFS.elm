module GTFS exposing (Accessibility(..), ExceptionType(..), Latitude, LocationType(..), Longitude, PathwayMode(..), PickupDropOffType(..), Timezone, dateToInt, locationTypeToString)

import Angle exposing (Angle)
import Date exposing (Date)


type alias Timezone =
    String


type alias Latitude =
    Angle


type alias Longitude =
    Angle



-------------------
-- Feed decoders --
-------------------


type PathwayMode
    = Walkway
    | Stairs
    | MovingSidewalk
    | Escalator
    | Elevator
    | FareGate
    | ExitGate


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
