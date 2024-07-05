module Types exposing (Feed, Id, Latitude, LocationType(..), Longitude, Model, Msg(..), OEvent(..), OStation, OTimetable, OViewMode(..), Stop, Timezone, WheelchairBoarding(..), parseLocationType, parseWheelchairBoarding)

import Dict exposing (Dict)
import Http
import RemoteData exposing (RemoteData)
import Time
import Url exposing (Url)


type alias Model =
    { timetable : OTimetable
    , mode : OViewMode
    , stops : RemoteData (Dict Feed (List Stop))
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
    | GotStops Feed (Result Http.Error (List Stop))
