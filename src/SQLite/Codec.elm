module SQLite.Codec exposing (Codec, andThen, angle, bool, boolToInt, clock, date, dateFromInt, dateToInt, float, id, int, kilometers, map, meters, parseBool, parsed, seconds, string, url)

import Angle exposing (Angle)
import Clock exposing (Clock)
import Csv.Decode
import Date exposing (Date)
import Duration exposing (Duration)
import Id exposing (Id)
import Json.Encode
import Length exposing (Length)
import Parser exposing (Parser)
import SQLite.Types
import Url exposing (Url)


type alias Codec a =
    { tipe : SQLite.Types.Type
    , encode : a -> Json.Encode.Value
    , decoder : Csv.Decode.Decoder a
    }


map : (a -> b) -> (b -> a) -> Codec a -> Codec b
map back go { tipe, encode, decoder } =
    { tipe = tipe
    , encode = \value -> encode (go value)
    , decoder = Csv.Decode.map back decoder
    }


andThen : (a -> Result String b) -> (b -> a) -> Codec a -> Codec b
andThen go back { tipe, encode, decoder } =
    { tipe = tipe
    , encode = \value -> encode (back value)
    , decoder =
        decoder
            |> Csv.Decode.andThen
                (\raw ->
                    case go raw of
                        Ok v ->
                            Csv.Decode.succeed v

                        Err e ->
                            Csv.Decode.fail e
                )
    }


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
    { tipe = SQLite.Types.Text
    , encode = Json.Encode.string
    , decoder = Csv.Decode.string
    }


float : Codec Float
float =
    { tipe = SQLite.Types.Real
    , encode = Json.Encode.float
    , decoder = Csv.Decode.float
    }


int : Codec Int
int =
    { tipe = SQLite.Types.Integer
    , encode = Json.Encode.int
    , decoder = Csv.Decode.int
    }
