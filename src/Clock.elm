module Clock exposing (Clock, duration, fromHoursMinutesSeconds, toHumanString, toString)

import Duration exposing (Duration, Seconds)
import Quantity exposing (Quantity(..))


{-| A time of day, represented as the number of seconds _from 12 hours before noon_.

Can be more than 24 hours for trip finishing the following day.

-}
type alias Clock =
    Quantity Int Seconds


toString : Clock -> String
toString (Quantity fromStartOfDay) =
    let
        allMinutes : Int
        allMinutes =
            fromStartOfDay // 60

        hour : Int
        hour =
            allMinutes // 60

        minute : Int
        minute =
            modBy 60 allMinutes

        second : Int
        second =
            modBy 60 fromStartOfDay

        pad : Int -> String
        pad x =
            x
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    pad hour ++ ":" ++ pad minute ++ ":" ++ pad second


toHumanString : Clock -> String
toHumanString (Quantity fromStartOfDay) =
    let
        allMinutes : Int
        allMinutes =
            fromStartOfDay // 60

        hour : Int
        hour =
            allMinutes // 60

        minute : Int
        minute =
            modBy 60 allMinutes

        pad : Int -> String
        pad x =
            x
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    if hour >= 24 then
        pad (modBy 24 hour) ++ ":" ++ pad minute ++ " (+" ++ String.fromInt (hour // 24) ++ ")"

    else
        pad hour ++ ":" ++ pad minute


duration : Clock -> Clock -> Duration
duration from to =
    Quantity.difference from to
        |> Quantity.toFloatQuantity


fromHoursMinutesSeconds : Int -> Int -> Int -> Clock
fromHoursMinutesSeconds h m s =
    Quantity (h * 3600 + m * 60 + s)
