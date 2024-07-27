module GTFS.SQLSource exposing
    ( Column
    , ColumnType(..)
    , Encoder
    , Table
    , TypedEncoder
    , accessibilityEncoder
    , accessibilityToInt
    , angleEncoder
    , boolEncoder
    , boolToInt
    , calendarDateEncoder
    , calendarEncoder
    , dateEncoder
    , dateToInt
    , dateToString
    , exceptionTypeEncoder
    , exceptionTypeToInt
    , float
    , id
    , int
    , length
    , locationTypeEncoder
    , locationTypeToInt
    , map
    , maybe
    , object
    , optional
    , pathwayEncoder
    , pathwayModeEncoder
    , pathwayModeToInt
    , pickupDropOffTypeEncoder
    , pickupDropOffTypeToInt
    , required
    , stopEncoder
    , stopTimeEncoder
    , string
    , timeEncoder
    , toCreate
    , toSqlColumn
    , toTableDefinition
    , tripEncoder
    , typeToSqlType
    , urlEncoder
    )

import Angle exposing (Angle)
import Clock exposing (Clock)
import Date exposing (Date)
import Duration
import GTFS exposing (Accessibility(..), Calendar, CalendarDate, ExceptionType(..), LocationType(..), Pathway, PathwayMode(..), PickupDropOffType(..), Stop, StopTime, Trip)
import Id exposing (Id)
import Json.Encode
import Length exposing (Length)
import SQLite.Statement as Statement
import SQLite.Statement.CreateTable as CreateTable
import SQLite.Types as Types
import Url exposing (Url)



-------------------
-- Feed Encoders --
-------------------


toCreate :
    { ifNotExists : Bool
    }
    -> Table a
    -> Statement.Statement
toCreate config table =
    Statement.CreateTable
        { name = table.name
        , ifNotExists = config.ifNotExists
        , temporary = False
        , schemaName = Nothing
        , definition = toTableDefinition table
        }


toTableDefinition : Table a -> CreateTable.TableDefinition
toTableDefinition table =
    CreateTable.TableDefinitionColumns
        { options =
            { strict = True
            , withoutRowid = False
            }
        , columns = List.map toSqlColumn table.columns
        , constraints =
            [ CreateTable.UnnamedTableConstraint
                (CreateTable.TablePrimaryKey
                    (List.map
                        (\name ->
                            { nameOrExpr = CreateTable.IsName name
                            , collate = Nothing
                            , ascDesc = Nothing
                            }
                        )
                        table.primaryKey
                    )
                    Nothing
                )
            ]
        }


toSqlColumn : Column -> CreateTable.ColumnDefinition
toSqlColumn column =
    { name = column.name
    , tipe = Just (typeToSqlType column.tipe)
    , constraints =
        case column.tipe of
            Nullable _ ->
                []

            _ ->
                [ CreateTable.UnnamedColumnConstraint (CreateTable.ColumnNotNull Nothing) ]
    }


typeToSqlType : ColumnType -> Types.Type
typeToSqlType tipe =
    case tipe of
        Nullable x ->
            typeToSqlType x

        Integer ->
            Types.Integer

        Real ->
            Types.Real

        Text ->
            Types.Text


type alias Table a =
    { name : String
    , primaryKey : List String
    , columns : List Column
    , encode : a -> Json.Encode.Value
    }


type alias Column =
    { name : String
    , tipe : ColumnType
    }


type alias TypedEncoder a =
    ( ColumnType, Encoder a )


type alias Encoder a =
    a -> Json.Encode.Value


type ColumnType
    = Integer
    | Real
    | Text
    | Nullable ColumnType


object : String -> List String -> List ( Column, Encoder a ) -> Table a
object name primaryKey children =
    { name = name
    , primaryKey = primaryKey
    , columns = List.map Tuple.first children
    , encode =
        \value ->
            children
                |> List.map (\( col, prop ) -> ( col.name, prop value ))
                |> Json.Encode.object
    }


stopTimeEncoder : Table StopTime
stopTimeEncoder =
    object "stop_times"
        [ "trip_id", "stop_sequence" ]
        [ required "trip_id" .trip_id id
        , optional "arrival_time" .arrival_time timeEncoder
        , optional "departure_time" .departure_time timeEncoder
        , optional "stop_id" .stop_id id
        , optional "location_group_id" .location_group_id id
        , optional "location_id" .location_id id
        , required "stop_sequence" .stop_sequence int
        , optional "stop_headsign" .stop_headsign string
        , optional "start_pickup_drop_off_window" .start_pickup_drop_off_window timeEncoder
        , optional "end_pickup_drop_off_window" .end_pickup_drop_off_window timeEncoder
        , optional "pickup_type" .pickup_type pickupDropOffTypeEncoder
        , optional "drop_off_type" .drop_off_type pickupDropOffTypeEncoder
        , optional "continuous_pickup" .continuous_pickup pickupDropOffTypeEncoder
        , optional "continuous_drop_off" .continuous_drop_off pickupDropOffTypeEncoder
        , optional "shape_dist_traveled" .shape_dist_traveled float
        , optional "timepoint" .timepoint boolEncoder
        , optional "pickup_booking_rule_id" .pickup_booking_rule_id id
        , optional "drop_off_booking_rule_id" .drop_off_booking_rule_id id
        ]


tripEncoder : Table Trip
tripEncoder =
    object "trips"
        [ "trip_id" ]
        [ required "route_id" .route_id id
        , required "service_id" .service_id id
        , required "trip_id" .id id
        , optional "trip_headsign" .headsign string
        , optional "trip_short_name" .short_name string
        , optional "direction_id" .direction_id boolEncoder
        , optional "block_id" .block_id id
        , optional "shape_id" .shape_id id
        , optional "wheelchair_accessible" .wheelchair_accessible accessibilityEncoder
        , optional "bikes_allowed" .bikes_allowed accessibilityEncoder
        ]


calendarEncoder : Table Calendar
calendarEncoder =
    object "calendar"
        [ "service_id" ]
        [ required "service_id" .id id
        , required "monday" .monday boolEncoder
        , required "tuesday" .tuesday boolEncoder
        , required "wednesday" .wednesday boolEncoder
        , required "thursday" .thursday boolEncoder
        , required "friday" .friday boolEncoder
        , required "saturday" .saturday boolEncoder
        , required "sunday" .sunday boolEncoder
        , required "start_date" .start_date dateEncoder
        , required "end_date" .end_date dateEncoder
        ]


calendarDateEncoder : Table CalendarDate
calendarDateEncoder =
    object "calendar_dates"
        [ "service_id", "date" ]
        [ required "service_id" .service_id id
        , required "date" .date dateEncoder
        , required "exception_type" .exception_type exceptionTypeEncoder
        ]


exceptionTypeEncoder : TypedEncoder ExceptionType
exceptionTypeEncoder =
    map exceptionTypeToInt int


exceptionTypeToInt : ExceptionType -> Int
exceptionTypeToInt input =
    case input of
        ServiceAdded ->
            1

        ServiceRemoved ->
            2


dateEncoder : TypedEncoder Date
dateEncoder =
    map dateToString string


dateToString : Date -> String
dateToString date =
    let
        pad : (Date -> Int) -> String
        pad prop =
            String.padLeft 2 '0' (String.fromInt (prop date))
    in
    pad Date.year ++ pad Date.monthNumber ++ pad Date.day


dateToInt : Date -> Int
dateToInt date =
    Date.year date * 10000 + Date.monthNumber date * 100 + Date.day date


pickupDropOffTypeEncoder : TypedEncoder PickupDropOffType
pickupDropOffTypeEncoder =
    map pickupDropOffTypeToInt int


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


stopEncoder : Table Stop
stopEncoder =
    object "stops"
        [ "stop_id" ]
        [ required "stop_id" .id id
        , optional "stop_code" .code string
        , optional "stop_name" .name string
        , optional "tts_stop_name" .tts_name string
        , optional "stop_desc" .description string
        , optional "stop_lat" .lat angleEncoder
        , optional "stop_lon" .lon angleEncoder
        , optional "zone_id" .zone_id id
        , optional "stop_url" .url urlEncoder
        , required "location_type" .location_type locationTypeEncoder
        , optional "parent_station" .parent_station id
        , optional "stop_timezone" .timezone string
        , optional "wheelchair_boarding" .wheelchair_boarding accessibilityEncoder
        , optional "level_id" .level_id id
        , optional "platform_code" .platform_code string
        ]


pathwayEncoder : Table Pathway
pathwayEncoder =
    object "pathways"
        [ "pathway_id" ]
        [ required "pathway_id" .id id
        , required "from_stop_id" .from_stop_id id
        , required "to_stop_id" .to_stop_id id
        , required "pathway_mode" .mode pathwayModeEncoder
        , required "is_bidirectional" .is_bidirectional boolEncoder
        , optional "length" .length length
        , optional "traversal_time" .traversal_time (map Duration.inSeconds float)
        , optional "stair_count" .stair_count int
        , optional "max_slope" .max_slope float
        , optional "min_width" .min_width length
        , optional "signposted_as" .signposted_as string
        , optional "reversed_signposted_as" .reversed_signposted_as string
        ]


locationTypeEncoder : TypedEncoder LocationType
locationTypeEncoder =
    map locationTypeToInt int


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


accessibilityEncoder : TypedEncoder Accessibility
accessibilityEncoder =
    map accessibilityToInt int


accessibilityToInt : Accessibility -> Int
accessibilityToInt input =
    case input of
        NoAccessibilityInformation ->
            0

        Accessibly ->
            1

        NotAccessible ->
            2


pathwayModeEncoder : TypedEncoder PathwayMode
pathwayModeEncoder =
    map pathwayModeToInt int


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



--------------------
-- Basic Encoders --
--------------------


id : TypedEncoder (Id kind)
id =
    map Id.toString string


timeEncoder : TypedEncoder Clock
timeEncoder =
    map Clock.toString string


boolEncoder : TypedEncoder Bool
boolEncoder =
    map boolToInt int


boolToInt : Bool -> Int
boolToInt input =
    if input then
        1

    else
        0


required :
    String
    -> (a -> p)
    -> TypedEncoder p
    -> ( Column, Encoder a )
required name getter ( tipe, encoder ) =
    ( { name = name
      , tipe = tipe
      }
    , \v -> encoder (getter v)
    )


optional :
    String
    -> (a -> Maybe p)
    -> TypedEncoder p
    -> ( Column, Encoder a )
optional name getter encoder =
    required name getter (maybe encoder)


length : TypedEncoder Length
length =
    map Length.inMeters float


angleEncoder : TypedEncoder Angle
angleEncoder =
    map Angle.inDegrees float


urlEncoder : TypedEncoder Url
urlEncoder =
    map Url.toString string


map : (b -> a) -> TypedEncoder a -> TypedEncoder b
map f ( tipe, encoder ) =
    ( tipe, \value -> encoder (f value) )


string : TypedEncoder String
string =
    ( Text, Json.Encode.string )


float : TypedEncoder Float
float =
    ( Real, Json.Encode.float )


int : TypedEncoder Int
int =
    ( Integer, Json.Encode.int )


maybe : TypedEncoder p -> TypedEncoder (Maybe p)
maybe ( tipe, encoder ) =
    ( Nullable tipe
    , \v ->
        case v of
            Nothing ->
                Json.Encode.null

            Just w ->
                encoder w
    )
