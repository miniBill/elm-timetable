module SQLite.Table exposing (Codec, Color, Column, ForeignKey, Table, TableBuilder, andThen, angle, bool, clock, color, date, dateToInt, float, id, int, kilometers, meters, seconds, string, table, url, with, withForeignKey, withPrimaryKey)

import Angle exposing (Angle)
import Clock exposing (Clock)
import Csv.Decode
import Date exposing (Date)
import Duration exposing (Duration)
import Id exposing (Id)
import Json.Encode
import Length exposing (Length)
import Parser exposing (Parser)
import SQLite.Statement.CreateTable exposing (ColumnDefinition)
import SQLite.Types
import Url exposing (Url)


type alias TableBuilder a ctor =
    { name : String
    , filename : String
    , columns : List ColumnDefinition
    , encode : a -> List ( String, Json.Encode.Value )
    , decoder : Csv.Decode.Decoder ctor
    }


type alias Table a =
    { name : String
    , filename : String
    , primaryKey : List String
    , columns : List ColumnDefinition
    , encode : a -> Json.Encode.Value
    , decoder : Csv.Decode.Decoder a
    , foreignKeys : List ForeignKey
    }


type alias Column a p =
    { definition : ColumnDefinition
    , encode : a -> Json.Encode.Value
    , decoder : Csv.Decode.Decoder p
    }


type alias ForeignKey =
    { columnNames : List String
    , tableName : String
    , mapsTo : Maybe (List String)
    }


type alias Codec a =
    ( SQLite.Types.Type
    , a -> Json.Encode.Value
    , Csv.Decode.Decoder a
    )


table : String -> String -> ctor -> TableBuilder a ctor
table filename name ctor =
    { name = name
    , filename = filename
    , columns = []
    , encode = \_ -> []
    , decoder = Csv.Decode.succeed ctor
    }


with : Column a p -> TableBuilder a (p -> b) -> TableBuilder a b
with column builder =
    { name = builder.name
    , filename = builder.filename
    , columns = column.definition :: builder.columns
    , encode = \v -> ( column.definition.name, column.encode v ) :: builder.encode v
    , decoder =
        builder.decoder
            |> Csv.Decode.pipeline column.decoder
    }


withPrimaryKey : List String -> TableBuilder a a -> Table a
withPrimaryKey primaryKey { name, filename, encode, decoder, columns } =
    { name = name
    , filename = filename
    , columns = List.reverse columns
    , encode =
        \value ->
            value
                |> encode
                |> Json.Encode.object
    , decoder = decoder
    , primaryKey = primaryKey
    , foreignKeys = []
    }


withForeignKey : ForeignKey -> Table a -> Table a
withForeignKey fk t =
    { t | foreignKeys = fk :: t.foreignKeys }


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


string : Codec String
string =
    ( SQLite.Types.Text, Json.Encode.string, Csv.Decode.string )


float : Codec Float
float =
    ( SQLite.Types.Real, Json.Encode.float, Csv.Decode.float )


int : Codec Int
int =
    ( SQLite.Types.Integer, Json.Encode.int, Csv.Decode.int )
