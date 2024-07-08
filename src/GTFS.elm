module GTFS exposing (Feed, Id, Latitude, LocationType(..), Longitude, Pathway, PathwayMode(..), Stop, StopTime, Time, Timezone, WheelchairBoarding(..), id, locationTypeParser, optional, parsed, pathwayDecoder, pathwayModeDecoder, required, stopDecoder, stopTimeDecoder, timeDecoder, timeParser, urlDecoder, weelchairBoardingDecoder)

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
    , wheelchair_boarding : Maybe WheelchairBoarding
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
        |> optional "wheelchair_boarding" weelchairBoardingDecoder
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


type WheelchairBoarding
    = NoWheelchairInfo
    | WheelchairAccessible
    | NotWheelchairAccessible


weelchairBoardingDecoder : Csv.Decode.Decoder WheelchairBoarding
weelchairBoardingDecoder =
    parsed weelchairBoardingParser


weelchairBoardingParser : String -> Maybe WheelchairBoarding
weelchairBoardingParser input =
    case input of
        "" ->
            Just NoWheelchairInfo

        "0" ->
            Just NoWheelchairInfo

        "1" ->
            Just WheelchairAccessible

        "2" ->
            Just NotWheelchairAccessible

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
        |= Parser.int
        |. Parser.symbol ":"
        |= Parser.int
        |. Parser.symbol ":"
        |= Parser.int


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
