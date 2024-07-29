module SQLite.Column exposing (Color, andThen, angle, bool, clock, color, date, float, id, int, kilometers, meters, notNull, nullable, seconds, string, url, withForeignKey, withForeignKeyTo)

import Angle exposing (Angle)
import Clock exposing (Clock)
import Csv.Decode
import Date exposing (Date)
import Duration exposing (Duration)
import Id exposing (Id)
import Json.Encode
import Length exposing (Length)
import Maybe.Extra
import Parser exposing (Parser)
import SQLite.Statement.CreateTable as CreateTable
import SQLite.Table exposing (Codec, Column)
import SQLite.Types
import Url exposing (Url)


notNull :
    String
    -> (a -> p)
    -> Codec p
    -> Column a p
notNull name getter ( tipe, encode, decoder ) =
    { definition =
        { name = name
        , tipe = Just tipe
        , constraints = [ { name = Nothing, constraint = CreateTable.ColumnNotNull Nothing } ]
        }
    , encode = \v -> encode (getter v)
    , decoder = Csv.Decode.field name decoder
    }


nullable :
    String
    -> (a -> Maybe p)
    -> Codec p
    -> Column a (Maybe p)
nullable name getter ( tipe, encode, decoder ) =
    { definition =
        { name = name
        , tipe = Just tipe
        , constraints = []
        }
    , encode =
        \v ->
            case getter v of
                Nothing ->
                    Json.Encode.null

                Just w ->
                    encode w
    , decoder =
        decoder
            |> Csv.Decode.blank
            |> Csv.Decode.optionalField name
            |> Csv.Decode.map Maybe.Extra.join
    }


withForeignKeyTo : { t | name : String } -> String -> Column a p -> Column a p
withForeignKeyTo { name } columnName ({ definition } as column) =
    { column
        | definition =
            { definition
                | constraints =
                    { name = Just ("to_" ++ name)
                    , constraint =
                        CreateTable.ColumnForeignKey
                            { foreignTable = name
                            , columnNames = [ columnName ]
                            , onDelete = Nothing
                            , onUpdate = Nothing
                            , match = Nothing
                            , defer = Nothing
                            }
                    }
                        :: definition.constraints
            }
    }


withForeignKey : { t | name : String } -> Column a p -> Column a p
withForeignKey { name } ({ definition } as column) =
    { column
        | definition =
            { definition
                | constraints =
                    { name = Just ("to_" ++ name)
                    , constraint =
                        CreateTable.ColumnForeignKey
                            { foreignTable = name
                            , columnNames = []
                            , onDelete = Nothing
                            , onUpdate = Nothing
                            , match = Nothing
                            , defer = Nothing
                            }
                    }
                        :: definition.constraints
            }
    }


map : (a -> b) -> (b -> a) -> Codec a -> Codec b
map back go ( tipe, encode, decoder ) =
    ( tipe
    , \value -> encode (go value)
    , Csv.Decode.map back decoder
    )


andThen : (a -> Result String b) -> (b -> a) -> Codec a -> Codec b
andThen go back ( tipe, encode, decoder ) =
    ( tipe
    , \value -> encode (back value)
    , decoder
        |> Csv.Decode.andThen
            (\raw ->
                case go raw of
                    Ok v ->
                        Csv.Decode.succeed v

                    Err e ->
                        Csv.Decode.fail e
            )
    )


parsed : String -> Parser a -> (a -> String) -> Codec String -> Codec a
parsed label parser go inner =
    andThen
        (\input ->
            case Parser.run parser input of
                Err _ ->
                    Err (input ++ " is not a valid " ++ label)

                Ok v ->
                    Ok v
        )
        go
        inner


date : Codec Date
date =
    map dateFromInt dateToInt int


dateFromInt : Int -> Date
dateFromInt input =
    let
        year : Int
        year =
            input // 10000

        month : Date.Month
        month =
            Date.numberToMonth (modBy 100 (input // 100))

        day : Int
        day =
            modBy 100 input
    in
    Date.fromCalendarDate year month day


dateToInt : Date -> Int
dateToInt input =
    Date.year input * 10000 + Date.monthNumber input * 100 + Date.day input


type alias Color =
    String


color : Codec Color
color =
    string


id : Codec (Id kind)
id =
    map Id.fromString Id.toString string


clock : Codec Clock
clock =
    parsed "time" Clock.parser Clock.toString string


seconds : Codec Duration
seconds =
    map Duration.seconds Duration.inSeconds float


meters : Codec Length
meters =
    map Length.meters Length.inMeters float


kilometers : Codec Length
kilometers =
    map Length.kilometers Length.inKilometers float


angle : Codec Angle
angle =
    map Angle.degrees Angle.inDegrees float


url : Codec Url
url =
    andThen
        (\v ->
            case Url.fromString v of
                Nothing ->
                    Err (v ++ " is not a valid URL")

                Just res ->
                    Ok res
        )
        Url.toString
        string


bool : Codec Bool
bool =
    andThen parseBool boolToInt int


parseBool : Int -> Result String Bool
parseBool input =
    case input of
        0 ->
            Ok False

        1 ->
            Ok True

        _ ->
            Err (String.fromInt input ++ " is not a valid bool")


boolToInt : Bool -> Int
boolToInt input =
    if input then
        1

    else
        0


string : Codec String
string =
    ( SQLite.Types.Text, Json.Encode.string, Csv.Decode.string )


float : Codec Float
float =
    ( SQLite.Types.Real, Json.Encode.float, Csv.Decode.float )


int : Codec Int
int =
    ( SQLite.Types.Integer, Json.Encode.int, Csv.Decode.int )
