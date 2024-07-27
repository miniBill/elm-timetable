module GTFS exposing
    ( Accessibility(..), ExceptionType(..), LocationType(..), PathwayMode(..), PickupDropOffType(..), Timezone
    , locationTypeToString
    , RouteType(..)
    )

{-|

@docs Accessibility, ExceptionType, LocationType, PathwayMode, PickupDropOffType, Timezone
@docs locationTypeToString

-}

-- Types --


type alias Timezone =
    String


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


type RouteType
    = TramStreetcarLightRail
    | SubwayMetro
    | Rail
    | Bus
    | Ferry
    | CableTram
    | AerialLift
    | Funicular
    | Trolleybus
    | Monorail



-- Tables --


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
