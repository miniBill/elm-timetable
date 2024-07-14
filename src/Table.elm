module Table exposing (angle, bool, clock, debug, duration, float, id, int, length, maybe, string, url)

import Angle exposing (Angle)
import Clock exposing (Clock)
import Duration
import Float.Extra
import Id exposing (Id)
import Length
import Ui exposing (Attribute)
import Ui.Font
import Ui.Table
import Url exposing (Url)


cell : List (Attribute msg) -> String -> Ui.Table.Cell msg
cell attrs s =
    Ui.Table.cell attrs (Ui.text s)


maybe : (a -> Ui.Table.Cell msg) -> Maybe a -> Ui.Table.Cell msg
maybe f x =
    case x of
        Just v ->
            f v

        Nothing ->
            string "---"


string : String -> Ui.Table.Cell msg
string s =
    cell [] s


angle : Angle -> Ui.Table.Cell msg
angle a =
    float (Angle.inDegrees a)


length : Length.Length -> Ui.Table.Cell msg
length l =
    string (String.fromFloat (Length.inMeters l) ++ "m")


duration : Duration.Duration -> Ui.Table.Cell msg
duration l =
    let
        raw : Float
        raw =
            Duration.inSeconds l

        s : Float
        s =
            Float.Extra.modBy 60 raw

        m : Int
        m =
            floor (raw / 60)
    in
    if m > 0 then
        cell
            [ Ui.Font.alignRight ]
            (String.fromInt m
                ++ "' "
                ++ String.fromFloat s
                ++ "\""
            )

    else
        cell
            [ Ui.Font.alignRight ]
            (String.fromFloat s ++ "\"")


bool : Bool -> Ui.Table.Cell msg
bool b =
    string
        (if b then
            "True"

         else
            "False"
        )


float : Float -> Ui.Table.Cell msg
float f =
    let
        precision : number
        precision =
            4

        rounded : Float
        rounded =
            toFloat (round (f * (10 ^ precision))) / (10 ^ precision)

        str : String
        str =
            String.fromFloat rounded
    in
    cell
        [ Ui.Font.alignRight ]
        (if String.contains "." str then
            String.padRight (3 + precision) '0' str

         else
            str
        )


int : Int -> Ui.Table.Cell msg
int f =
    cell
        [ Ui.Font.alignRight ]
        (String.fromInt f)


url : Url -> Ui.Table.Cell msg
url v =
    string (Url.toString v)


debug : a -> Ui.Table.Cell msg
debug v =
    string (Debug.toString v)


clock : Clock -> Ui.Table.Cell msg
clock t =
    string (Clock.toHumanString t)


id : Id kind -> Ui.Table.Cell msg
id v =
    string (Id.toString v)
