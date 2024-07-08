module Table exposing (angle, cell, debug, duration, float, int, length, maybe, seconds, string, url)

import Angle exposing (Angle)
import Duration exposing (Seconds)
import Float.Extra
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Length
import Quantity exposing (Quantity)
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


float : Float -> Html msg
float f =
    cell
        [ style "text-align" "right" ]
        (String.fromFloat f)


seconds : Quantity Int Seconds -> Html msg
seconds f =
    duration (Duration.seconds (toFloat (Quantity.unwrap f)))


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
