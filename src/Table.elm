module Table exposing (angle, bool, debug, duration, float, id, int, length, maybe, string, time, url)

import Angle exposing (Angle)
import Duration
import Float.Extra
import GTFS exposing (Time)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Id exposing (Id)
import Length
import Url exposing (Url)


cell : List (Attribute msg) -> String -> Html msg
cell attrs s =
    Html.td attrs [ Html.text s ]


maybe : (a -> Html msg) -> Maybe a -> Html msg
maybe f x =
    case x of
        Just v ->
            f v

        Nothing ->
            string "---"


string : String -> Html msg
string s =
    cell [] s


angle : Angle -> Html msg
angle a =
    float (Angle.inDegrees a)


length : Length.Length -> Html msg
length l =
    string (String.fromFloat (Length.inMeters l) ++ "m")


duration : Duration.Duration -> Html msg
duration l =
    let
        raw =
            Duration.inSeconds l

        s =
            Float.Extra.modBy 60 raw

        m =
            floor (raw / 60)
    in
    if m > 0 then
        cell
            [ style "text-align" "right" ]
            (String.fromInt m
                ++ "' "
                ++ String.fromFloat s
                ++ "\""
            )

    else
        cell
            [ style "text-align" "right" ]
            (String.fromFloat s ++ "\"")


bool : Bool -> Html msg
bool b =
    string
        (if b then
            "True"

         else
            "False"
        )


float : Float -> Html msg
float f =
    cell
        [ style "text-align" "right" ]
        (String.fromFloat f)


int : Int -> Html msg
int f =
    cell
        [ style "text-align" "right" ]
        (String.fromInt f)


url : Url -> Html msg
url v =
    string (Url.toString v)


debug : a -> Html msg
debug v =
    string (Debug.toString v)


time : Time -> Html msg
time t =
    string (GTFS.timeToHumanString t)


id : Id kind -> Html msg
id v =
    string (Id.toString v)
