module GTFS exposing (Accessibility(..), Feed, Id, Latitude, LocationType(..), Longitude, Pathway, PathwayMode(..), Stop, StopTime, Time, Timezone, Trip, accessibilityDecoder, id, locationTypeParser, optional, parsed, pathwayDecoder, pathwayModeDecoder, required, stopDecoder, stopTimeDecoder, timeDecoder, timeParser, timeToString, tripDecoder, urlDecoder)

import Angle exposing (Angle)
import Csv.Decode
import Duration exposing (Seconds)
import Length exposing (Length)
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)
import Quantity exposing (Quantity)
import Url exposing (Url)


type alias Id =
    String


type alias Feed =
    Id


type alias Timezone =
    String


type alias Latitude =
    Angle


type alias Longitude =
    Angle


{-| A time of day, represented as the offset _from noon_.

Can be negative for mornings, and can be more than 12 hours for trip finishing the following day.

-}
type alias Time =
    Quantity Int Seconds



-------------------
-- Feed decoders --
-------------------


type alias StopTime =
    { trip_id : Id
    , arrival_time : Maybe Time
    , departure_time : Maybe Time
    , stop_id : Maybe Id
    , location_group_id : Maybe Id
    , location_id : Maybe Id
    , stop_sequence : Int
    , stop_headsign : Maybe String
    , start_pickup_drop_off_window : Maybe Time
    , end_pickup_drop_off_window : Maybe Time
    , pickup_type : Maybe PickupDropOffType
    , drop_off_type : Maybe PickupDropOffType
    , continuous_pickup : Maybe PickupDropOffType
    , continuous_drop_off : Maybe PickupDropOffType
    , shape_dist_traveled : Maybe Float

    -- False for approximate, True or Nothing for exact
    , timepoint : Maybe Bool
    , pickup_booking_rule_id : Maybe Id
    , drop_off_booking_rule_id : Maybe Id
    }


stopTimeDecoder : Csv.Decode.Decoder StopTime
stopTimeDecoder =
    Csv.Decode.succeed StopTime
        |> required "trip_id" id
        |> optional "arrival_time" timeDecoder
        |> optional "departure_time" timeDecoder
        |> optional "stop_id" id
        |> optional "location_group_id" id
        |> optional "location_id" id
        |> required "stop_sequence" Csv.Decode.int
        |> optional "stop_headsign" Csv.Decode.string
        |> optional "start_pickup_drop_off_window" timeDecoder
        |> optional "end_pickup_drop_off_window" timeDecoder
        |> optional "pickup_type" pickupDropOffTypeDecoder
        |> optional "drop_off_type" pickupDropOffTypeDecoder
        |> optional "continuous_pickup" pickupDropOffTypeDecoder
        |> optional "continuous_drop_off" pickupDropOffTypeDecoder
        |> optional "shape_dist_traveled" Csv.Decode.float
        |> optional "timepoint" boolDecoder
        |> optional "pickup_booking_rule_id" id
        |> optional "drop_off_booking_rule_id" id


type alias Trip =
    { route_id : Id
    , service_id : Id
    , id : Id
    , headsign : Maybe String
    , short_name : Maybe String
    , direction_id : Maybe Bool
    , block_id : Maybe Id
    , shape_id : Maybe Id
    , wheelchair_accessible : Maybe Accessibility
    , bikes_allowed : Maybe Accessibility
    }


tripDecoder : Csv.Decode.Decoder Trip
tripDecoder =
    Csv.Decode.succeed Trip
        |> required "route_id" id
        |> required "service_id" id
        |> required "trip_id" id
        |> optional "trip_headsign" Csv.Decode.string
        |> optional "trip_short_name" Csv.Decode.string
        |> optional "direction_id" boolDecoder
        |> optional "block_id" id
        |> optional "shape_id" id
        |> optional "wheelchair_accessible" accessibilityDecoder
        |> optional "bikes_allowed" accessibilityDecoder


type PickupDropOffType
    = RegularlyScheduled
    | NoPickupDropOff
    | PhoneAgency
    | CoordinateWithDriver


pickupDropOffTypeDecoder : Csv.Decode.Decoder PickupDropOffType
pickupDropOffTypeDecoder =
    parsed pickupDropOffTypeParser


pickupDropOffTypeParser : String -> Maybe PickupDropOffType
pickupDropOffTypeParser input =
    case input of
        "" ->
            Just RegularlyScheduled

        "0" ->
            Just RegularlyScheduled

        "1" ->
            Just NoPickupDropOff

        "2" ->
            Just PhoneAgency

        "3" ->
            Just CoordinateWithDriver

        _ ->
            Nothing


type alias Stop =
    { id : Id
    , code : Maybe String
    , name : Maybe String
    , tts_name : Maybe String
    , description : Maybe String
    , lat : Maybe Latitude
    , lon : Maybe Longitude
    , zone_id : Maybe Id
    , url : Maybe Url
    , location_type : LocationType
    , parent_station : Maybe Id
    , timezone : Maybe Timezone
    , wheelchair_boarding : Maybe Accessibility
    , level_id : Maybe Id
    , platform_code : Maybe String
    }


stopDecoder : Csv.Decode.Decoder Stop
stopDecoder =
    Csv.Decode.succeed Stop
        |> required "stop_id" id
        |> optional "stop_code" Csv.Decode.string
        |> optional "stop_name" Csv.Decode.string
        |> optional "tts_stop_name" Csv.Decode.string
        |> optional "stop_desc" Csv.Decode.string
        |> optional "stop_lat" angleDecoder
        |> optional "stop_lon" angleDecoder
        |> optional "zone_id" id
        |> optional "stop_url" urlDecoder
        |> required "location_type" locationTypeDecoder
        |> optional "parent_station" id
        |> optional "stop_timezone" Csv.Decode.string
        |> optional "wheelchair_boarding" accessibilityDecoder
        |> optional "level_id" id
        |> optional "platform_code" Csv.Decode.string


type alias Pathway =
    { id : Id
    , from_stop_id : Id
    , to_stop_id : Id
    , mode : PathwayMode
    , is_bidirectional : Bool
    , length : Maybe Length
    , traversal_time : Maybe (Quantity Int Seconds)
    , stair_count : Maybe Int
    , max_slope : Maybe Float
    , min_width : Maybe Length
    , signposted_as : Maybe String
    , reversed_signposted_as : Maybe String
    }


pathwayDecoder : Csv.Decode.Decoder Pathway
pathwayDecoder =
    Csv.Decode.succeed Pathway
        |> required "pathway_id" id
        |> required "from_stop_id" id
        |> required "to_stop_id" id
        |> required "pathway_mode" pathwayModeDecoder
        |> required "is_bidirectional" boolDecoder
        |> optional "length" length
        |> optional "traversal_time" (Csv.Decode.map Quantity.unsafe Csv.Decode.int)
        |> optional "stair_count" Csv.Decode.int
        |> optional "max_slope" Csv.Decode.float
        |> optional "min_width" length
        |> optional "signposted_as" Csv.Decode.string
        |> optional "reversed_signposted_as" Csv.Decode.string


type LocationType
    = StopPlatform
    | Station
    | EntranceExit
    | GenericNode
    | BoardingArea


locationTypeDecoder : Csv.Decode.Decoder LocationType
locationTypeDecoder =
    parsed locationTypeParser


locationTypeParser : String -> Maybe LocationType
locationTypeParser input =
    case input of
        "" ->
            Just StopPlatform

        "0" ->
            Just StopPlatform

        "1" ->
            Just Station

        "2" ->
            Just EntranceExit

        "3" ->
            Just GenericNode

        "4" ->
            Just BoardingArea

        _ ->
            Nothing


type Accessibility
    = NoAccessibilityInformation
    | Accessibly
    | NotAccessible


accessibilityDecoder : Csv.Decode.Decoder Accessibility
accessibilityDecoder =
    parsed accessibilityParser


accessibilityParser : String -> Maybe Accessibility
accessibilityParser input =
    case input of
        "" ->
            Just NoAccessibilityInformation

        "0" ->
            Just NoAccessibilityInformation

        "1" ->
            Just Accessibly

        "2" ->
            Just NotAccessible

        _ ->
            Nothing


type PathwayMode
    = Walkway
    | Stairs
    | MovingSidewalk
    | Escalator
    | Elevator
    | FareGate
    | ExitGate


pathwayModeDecoder : Csv.Decode.Decoder PathwayMode
pathwayModeDecoder =
    parsed pathwayModeParser


pathwayModeParser : String -> Maybe PathwayMode
pathwayModeParser input =
    case input of
        "1" ->
            Just Walkway

        "2" ->
            Just Stairs

        "3" ->
            Just MovingSidewalk

        "4" ->
            Just Escalator

        "5" ->
            Just Elevator

        "6" ->
            Just FareGate

        "7" ->
            Just ExitGate

        _ ->
            Nothing



--------------------
-- Basic decoders --
--------------------


id : Csv.Decode.Decoder Id
id =
    Csv.Decode.string


timeDecoder : Csv.Decode.Decoder Time
timeDecoder =
    parsed timeParser


timeParser : String -> Maybe Time
timeParser input =
    input
        |> Parser.run timeInnerParser
        |> Result.toMaybe


timeInnerParser : Parser Time
timeInnerParser =
    let
        noon : Int
        noon =
            12 * 60 * 60

        int =
            (Parser.chompIf Char.isDigit
                |. Parser.chompWhile Char.isDigit
            )
                |> Parser.getChompedString
                |> Parser.andThen
                    (\r ->
                        case String.toInt r of
                            Just i ->
                                Parser.succeed i

                            Nothing ->
                                Parser.problem (r ++ " is not a valid number")
                    )
    in
    Parser.succeed
        (\h m s ->
            let
                raw : Int
                raw =
                    h * 3600 + m * 60 + s

                offset : Int
                offset =
                    raw - noon
            in
            Quantity.unsafe offset
        )
        |= int
        |. Parser.symbol ":"
        |= int
        |. Parser.symbol ":"
        |= int


timeToString : Time -> String
timeToString t =
    let
        fromStartOfDay : Int
        fromStartOfDay =
            12 * 60 * 60 + Quantity.unwrap t

        allMinutes : Int
        allMinutes =
            fromStartOfDay // 60

        hour : Int
        hour =
            allMinutes // 60

        minute : Int
        minute =
            modBy 60 allMinutes

        second : Int
        second =
            modBy 60 fromStartOfDay

        pad : Int -> String
        pad x =
            x
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    pad hour ++ ":" ++ pad minute ++ ":" ++ pad second


boolDecoder : Csv.Decode.Decoder Bool
boolDecoder =
    parsed boolParser


boolParser : String -> Maybe Bool
boolParser input =
    case input of
        "0" ->
            Just False

        "1" ->
            Just True

        _ ->
            Nothing


length : Csv.Decode.Decoder Length
length =
    Csv.Decode.map Length.meters Csv.Decode.float


angleDecoder : Csv.Decode.Decoder Angle
angleDecoder =
    Csv.Decode.map Angle.degrees Csv.Decode.float


urlDecoder : Csv.Decode.Decoder Url
urlDecoder =
    parsed Url.fromString


required :
    String
    -> Csv.Decode.Decoder a
    -> Csv.Decode.Decoder (a -> b)
    -> Csv.Decode.Decoder b
required name decoder original =
    Csv.Decode.pipeline (Csv.Decode.field name decoder) original


optional :
    String
    -> Csv.Decode.Decoder a
    -> Csv.Decode.Decoder (Maybe a -> b)
    -> Csv.Decode.Decoder b
optional name decoder original =
    Csv.Decode.pipeline
        (decoder
            |> Csv.Decode.blank
            |> Csv.Decode.optionalField name
            |> Csv.Decode.map Maybe.Extra.join
        )
        original


parsed : (String -> Maybe a) -> Csv.Decode.Decoder a
parsed validation =
    Csv.Decode.string
        |> Csv.Decode.andThen
            (\raw ->
                case validation raw of
                    Just url ->
                        Csv.Decode.succeed url

                    Nothing ->
                        Csv.Decode.fail "Failed to parse"
            )
