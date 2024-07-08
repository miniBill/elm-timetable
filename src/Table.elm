module Table exposing (cell, debug, duration, float, int, length, maybe, string, url)

import Duration
import Float.Extra
import Html exposing (Html)
import Length
import Url exposing (Url)


cell : String -> Html msg
cell s =
    Html.td [] [ Html.text s ]


maybe : (a -> Html msg) -> Maybe a -> Html msg
maybe f x =
    case x of
        Just v ->
            f v

        Nothing ->
            cell "---"


string : String -> Html msg
string s =
    cell s


length : Length.Length -> Html msg
length l =
    cell (String.fromFloat (Length.inMeters l) ++ "m")


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
            (String.fromInt m
                ++ "' "
                ++ String.fromFloat s
                ++ "\""
            )

    else
        cell (String.fromFloat s ++ "\"")


float : Float -> Html msg
float f =
    cell (String.fromFloat f)


int : Int -> Html msg
int f =
    cell (String.fromInt f)


url : Url -> Html msg
url v =
    cell (Url.toString v)


debug : a -> Html msg
debug v =
    cell (Debug.toString v)
