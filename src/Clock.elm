module Clock exposing (Clock, fromHoursMinutesSeconds, parser, toHumanString, toString)

import Duration exposing (Seconds)
import Parser exposing ((|.), (|=), Parser)
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


fromHoursMinutesSeconds : Int -> Int -> Int -> Clock
fromHoursMinutesSeconds h m s =
    Quantity (h * 3600 + m * 60 + s)


parser : Parser Clock
parser =
    Parser.succeed fromHoursMinutesSeconds
        |= intParser
        |. Parser.symbol ":"
        |= intParser
        |. Parser.symbol ":"
        |= intParser


intParser : Parser Int
intParser =
    (Parser.chompIf Char.isDigit
        |. Parser.chompWhile Char.isDigit
    )
        |> Parser.getChompedString
        |> Parser.andThen
            (\r ->
                case String.toInt r of
                    Just i ->
                        Parser.succeed i

                    Nothing ->
                        Parser.problem (r ++ " is not a valid number")
            )
