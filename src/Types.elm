module Types exposing (Feed, Id, Latitude, LocationType(..), Longitude, Model, Msg(..), OEvent(..), OStation, OTimetable, OViewMode(..), Pathway, PathwayMode(..), Stop, Timezone, WheelchairBoarding(..), parseLocationType, parsePathwayMode, parseWheelchairBoarding)

import Dict exposing (Dict)
import Duration exposing (Duration)
import Http
import Length exposing (Length)
import RemoteData exposing (RemoteData)
import Time
import Url exposing (Url)


type alias Model =
    { timetable : OTimetable
    , mode : OViewMode
    , stops : RemoteData (Dict Feed (Dict Id Stop))
    , pathways : RemoteData (Dict Feed (Dict Id Pathway))
    }


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


type LocationType
    = StopPlatform
    | Station
    | EntranceExit
    | GenericNode
    | BoardingArea


parseLocationType : String -> Maybe LocationType
parseLocationType input =
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


parseWheelchairBoarding : String -> Maybe WheelchairBoarding
parseWheelchairBoarding input =
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


type alias Pathway =
    { id : Id
    , from_stop_id : Id
    , to_stop_id : Id
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


type PathwayMode
    = Walkway
    | Stairs
    | MovingSidewalk
    | Escalator
    | Elevator
    | FareGate
    | ExitGate


parsePathwayMode : String -> Maybe PathwayMode
parsePathwayMode input =
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


type alias Timezone =
    String


type alias Id =
    String


type alias Feed =
    String


type alias Latitude =
    Float


type alias Longitude =
    Float


type OViewMode
    = ViewSimple


type alias OTimetable =
    List
        { from : OStation
        , to : OStation
        , links : List { from : Time.Posix, to : Time.Posix }
        }


type alias OStation =
    String


type OEvent
    = Arrival
    | Departure


type Msg
    = OViewMode OViewMode
    | Reload
    | GotStops Feed (Result Http.Error (Dict Id Stop))
    | GotPathways Feed (Result Http.Error (Dict Id Pathway))
