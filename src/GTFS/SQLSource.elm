module GTFS.SQLSource exposing
    ( accessibilityToInt
    , calendarDatesTable
    , calendarTable
    , exceptionTypeEncoder
    , exceptionTypeToInt
    , locationTypeEncoder
    , locationTypeToInt
    , pathwayMode
    , pathwayModeToInt
    , pathwaysTable
    , pickupDropOffType
    , pickupDropOffTypeToInt
    , stopTimesTable
    , stopsTable
    , toCreate
    , toSqlColumn
    , toTableDefinition
    , tripsTable
    , typeToSqlType
    )

import Angle exposing (Angle)
import GTFS exposing (Accessibility(..), Calendar, CalendarDate, ExceptionType(..), LocationType(..), Pathway, PathwayMode(..), PickupDropOffType(..), Stop, StopTime, Timezone, Trip)
import Id exposing (AgencyId, Id, NetworkId, RouteId, ShapeId)
import Length exposing (Length)
import SQLite.Statement as Statement
import SQLite.Statement.CreateTable as CreateTable
import SQLite.TableBuilder exposing (Codec, Color, Column, ColumnType(..), Table, andThen, angle, bool, clock, color, column, date, float, id, int, kilometers, meters, nullColumn, seconds, string, table, url, withForeignKey, withPrimaryKey)
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
toCreate config t =
    Statement.CreateTable
        { name = t.name
        , ifNotExists = config.ifNotExists
        , temporary = False
        , schemaName = Nothing
        , definition = toTableDefinition t
        }


toTableDefinition : Table a -> CreateTable.TableDefinition
toTableDefinition { columns, primaryKey } =
    CreateTable.TableDefinitionColumns
        { options =
            { strict = True
            , withoutRowid = False
            }
        , columns = feedColumn :: List.map toSqlColumn columns
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
                        (feedColumn.name :: primaryKey)
                    )
                    Nothing
                )
            ]
        }


feedColumn : CreateTable.ColumnDefinition
feedColumn =
    { name = "feed"
    , tipe = Just Types.Text
    , constraints =
        [ CreateTable.UnnamedColumnConstraint (CreateTable.ColumnNotNull Nothing) ]
    }


toSqlColumn : Column -> CreateTable.ColumnDefinition
toSqlColumn { name, tipe } =
    { name = name
    , tipe = Just (typeToSqlType tipe)
    , constraints =
        case tipe of
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


stopTimesTable : Table StopTime
stopTimesTable =
    table "stop_times.txt" "stop_times" StopTime
        |> column "trip_id" .trip_id id
        |> nullColumn "arrival_time" .arrival_time clock
        |> nullColumn "departure_time" .departure_time clock
        |> nullColumn "stop_id" .stop_id id
        |> nullColumn "location_group_id" .location_group_id id
        |> nullColumn "location_id" .location_id id
        |> column "stop_sequence" .stop_sequence int
        |> nullColumn "stop_headsign" .stop_headsign string
        |> nullColumn "start_pickup_drop_off_window" .start_pickup_drop_off_window clock
        |> nullColumn "end_pickup_drop_off_window" .end_pickup_drop_off_window clock
        |> nullColumn "pickup_type" .pickup_type pickupDropOffType
        |> nullColumn "drop_off_type" .drop_off_type pickupDropOffType
        |> nullColumn "continuous_pickup" .continuous_pickup pickupDropOffType
        |> nullColumn "continuous_drop_off" .continuous_drop_off pickupDropOffType
        |> nullColumn "shape_dist_traveled" .shape_dist_traveled float
        |> nullColumn "timepoint" .timepoint bool
        |> nullColumn "pickup_booking_rule_id" .pickup_booking_rule_id id
        |> nullColumn "drop_off_booking_rule_id" .drop_off_booking_rule_id id
        |> withPrimaryKey [ "trip_id", "stop_sequence" ]
        |> withForeignKey { columnName = "trip_id", tableName = "trips", mapsTo = Nothing }
        |> withForeignKey { columnName = "stop_id", tableName = "stops", mapsTo = Nothing }
        |> withForeignKey { columnName = "location_group_id", tableName = "location_groups", mapsTo = Nothing }
        |> withForeignKey { columnName = "location_id", tableName = "locations", mapsTo = Nothing }
        |> withForeignKey { columnName = "pickup_booking_rule_id", tableName = "booking_rules", mapsTo = Nothing }
        |> withForeignKey { columnName = "drop_off_booking_rule_id", tableName = "booking_rules", mapsTo = Nothing }


tripsTable : Table Trip
tripsTable =
    table "trips.txt" "trips" Trip
        |> column "route_id" .route_id id
        |> column "service_id" .service_id id
        |> column "trip_id" .id id
        |> nullColumn "trip_headsign" .headsign string
        |> nullColumn "trip_short_name" .short_name string
        |> nullColumn "direction_id" .direction_id bool
        |> nullColumn "block_id" .block_id id
        |> nullColumn "shape_id" .shape_id id
        |> nullColumn "wheelchair_accessible" .wheelchair_accessible accessibilityEncoder
        |> nullColumn "bikes_allowed" .bikes_allowed accessibilityEncoder
        |> withPrimaryKey [ "trip_id" ]
        |> withForeignKey { columnName = "route_id", tableName = routesTable.name, mapsTo = Nothing }
        |> withForeignKey { columnName = "shape_id", tableName = shapePointsTable.name, mapsTo = Nothing }


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


routesTable : Table Route
routesTable =
    table "routes.txt" "routes" Route
        |> column "route_id" .id id
        |> nullColumn "agency_id" .agency_id id
        |> nullColumn "route_short_name" .short_name string
        |> nullColumn "route_long_name" .long_name string
        |> nullColumn "route_desc" .description string
        |> column "route_type" .tipe routeType
        |> nullColumn "route_url" .url string
        |> nullColumn "route_color" .color color
        |> nullColumn "route_text_color" .text_color color
        |> nullColumn "route_sort_order" .sort_order int
        |> nullColumn "continuous_pickup" .continuous_pickup pickupDropOffType
        |> nullColumn "continuous_drop_off" .continuous_drop_off pickupDropOffType
        |> nullColumn "network_id" .network_id id
        |> withPrimaryKey [ "route_id" ]
        |> withForeignKey { columnName = "agency_id", tableName = agencyTable.name, mapsTo = Nothing }


type alias ShapePoint =
    { shape_id : Id ShapeId
    , latitude : Angle
    , longitude : Angle
    , sequence : Int
    , distance_traveled : Maybe Length
    }


shapePointsTable : Table ShapePoint
shapePointsTable =
    table "shapes.txt" "shape_points" ShapePoint
        |> column "shape_id" .shape_id id
        |> column "shape_pt_lat" .latitude angle
        |> column "shape_pt_lon" .longitude angle
        |> column "shape_pt_sequence" .sequence int
        |> nullColumn "shape_dist_traveled" .distance_traveled kilometers
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


agencyTable : Table Agency
agencyTable =
    table "agency.txt" "agencies" Agency
        |> column "agency_id" .id id
        |> column "agency_name" .name string
        |> column "agency_url" .url url
        |> column "agency_timezone" .timezone string
        |> nullColumn "agency_lang" .lang string
        |> nullColumn "agency_phone" .phone string
        |> nullColumn "agency_fare_url" .fare_url string
        |> nullColumn "agency_email" .email string
        |> withPrimaryKey [ "agency_id" ]


calendarTable : Table Calendar
calendarTable =
    table "calendar.txt" "calendars" Calendar
        |> column "service_id" .id id
        |> column "monday" .monday bool
        |> column "tuesday" .tuesday bool
        |> column "wednesday" .wednesday bool
        |> column "thursday" .thursday bool
        |> column "friday" .friday bool
        |> column "saturday" .saturday bool
        |> column "sunday" .sunday bool
        |> column "start_date" .start_date date
        |> column "end_date" .end_date date
        |> withPrimaryKey [ "service_id" ]


calendarDatesTable : Table CalendarDate
calendarDatesTable =
    table "calendar_dates.txt" "calendar_dates" CalendarDate
        |> column "service_id" .service_id id
        |> column "date" .date date
        |> column "exception_type" .exception_type exceptionTypeEncoder
        |> withPrimaryKey [ "service_id", "date" ]


exceptionTypeEncoder : Codec ExceptionType
exceptionTypeEncoder =
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


stopsTable : Table Stop
stopsTable =
    table "stops.txt" "stops" Stop
        |> column "stop_id" .id id
        |> nullColumn "stop_code" .code string
        |> nullColumn "stop_name" .name string
        |> nullColumn "tts_stop_name" .tts_name string
        |> nullColumn "stop_desc" .description string
        |> nullColumn "stop_lat" .lat angle
        |> nullColumn "stop_lon" .lon angle
        |> nullColumn "zone_id" .zone_id id
        |> nullColumn "stop_url" .url url
        |> column "location_type" .location_type locationTypeEncoder
        |> nullColumn "parent_station" .parent_station id
        |> nullColumn "stop_timezone" .timezone string
        |> nullColumn "wheelchair_boarding" .wheelchair_boarding accessibilityEncoder
        |> nullColumn "level_id" .level_id id
        |> nullColumn "platform_code" .platform_code string
        |> withPrimaryKey [ "stop_id" ]
        |> withForeignKey { columnName = "parent_station", tableName = "stops", mapsTo = Just "stop_id" }
        |> withForeignKey { columnName = "level_id", tableName = "levels", mapsTo = Nothing }


pathwaysTable : Table Pathway
pathwaysTable =
    table "pathways.txt" "pathways" Pathway
        |> column "pathway_id" .id id
        |> column "from_stop_id" .from_stop_id id
        |> column "to_stop_id" .to_stop_id id
        |> column "pathway_mode" .mode pathwayMode
        |> column "is_bidirectional" .is_bidirectional bool
        |> nullColumn "length" .length meters
        |> nullColumn "traversal_time" .traversal_time seconds
        |> nullColumn "stair_count" .stair_count int
        |> nullColumn "max_slope" .max_slope float
        |> nullColumn "min_width" .min_width meters
        |> nullColumn "signposted_as" .signposted_as string
        |> nullColumn "reversed_signposted_as" .reversed_signposted_as string
        |> withPrimaryKey [ "pathway_id" ]
        |> withForeignKey { columnName = "from_stop_id", tableName = stopsTable.name, mapsTo = Just "stop_id" }
        |> withForeignKey { columnName = "to_stop_id", tableName = stopsTable.name, mapsTo = Just "stop_id" }


locationTypeEncoder : Codec LocationType
locationTypeEncoder =
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


accessibilityEncoder : Codec Accessibility
accessibilityEncoder =
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



--------------------
-- Basic Encoders --
--------------------


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
