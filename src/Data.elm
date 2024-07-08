module Data exposing (feeds, fromStringUnsafe, pathwayDecoder, stopsDecoder, villachToUdine)

import Csv.Decode
import Duration
import Iso8601
import Length
import Maybe.Extra
import Time
import Types exposing (OTimetable, Pathway, Stop)
import Url exposing (Url)


feeds : List String
feeds =
    [ --  "de" ,
      "oebb-2024"
    ]


stopsDecoder : Csv.Decode.Decoder Stop
stopsDecoder =
    Csv.Decode.succeed Stop
        |> required "stop_id" Csv.Decode.string
        |> optional "stop_code" Csv.Decode.string
        |> optional "stop_name" Csv.Decode.string
        |> optional "tts_stop_name" Csv.Decode.string
        |> optional "stop_desc" Csv.Decode.string
        |> optional "stop_lat" Csv.Decode.float
        |> optional "stop_lon" Csv.Decode.float
        |> optional "zone_id" Csv.Decode.string
        |> optional "stop_url" urlParser
        |> required "location_type" (parsed Types.parseLocationType)
        |> optional "parent_station" Csv.Decode.string
        |> optional "stop_timezone" Csv.Decode.string
        |> optional "wheelchair_boarding" (parsed Types.parseWheelchairBoarding)
        |> optional "level_id" Csv.Decode.string
        |> optional "platform_code" Csv.Decode.string


pathwayDecoder : Csv.Decode.Decoder Pathway
pathwayDecoder =
    Csv.Decode.succeed Pathway
        |> required "pathway_id" Csv.Decode.string
        |> required "from_stop_id" Csv.Decode.string
        |> required "to_stop_id" Csv.Decode.string
        |> required "pathway_mode" (parsed Types.parsePathwayMode)
        |> required "is_bidirectional"
            (parsed
                (\i ->
                    case i of
                        "0" ->
                            Just False

                        "1" ->
                            Just True

                        _ ->
                            Nothing
                )
            )
        |> optional "length" (Csv.Decode.map Length.meters Csv.Decode.float)
        |> optional "traversal_time" (Csv.Decode.map Duration.seconds Csv.Decode.float)
        |> optional "stair_count" Csv.Decode.int
        |> optional "max_slope" Csv.Decode.float
        |> optional "min_width" (Csv.Decode.map Length.meters Csv.Decode.float)
        |> optional "signposted_as" Csv.Decode.string
        |> optional "reversed_signposted_as" Csv.Decode.string


urlParser : Csv.Decode.Decoder Url
urlParser =
    parsed Url.fromString


required :
    String
    -> Csv.Decode.Decoder a
    -> Csv.Decode.Decoder (a -> b)
    -> Csv.Decode.Decoder b
required name decoder original =
    Csv.Decode.pipeline (Csv.Decode.field name decoder) original


optional :
    String
    -> Csv.Decode.Decoder a
    -> Csv.Decode.Decoder (Maybe a -> b)
    -> Csv.Decode.Decoder b
optional name decoder original =
    Csv.Decode.pipeline
        (decoder
            |> Csv.Decode.blank
            |> Csv.Decode.optionalField name
            |> Csv.Decode.map Maybe.Extra.join
        )
        original


parsed : (String -> Maybe a) -> Csv.Decode.Decoder a
parsed validation =
    Csv.Decode.string
        |> Csv.Decode.andThen
            (\raw ->
                case validation raw of
                    Just url ->
                        Csv.Decode.succeed url

                    Nothing ->
                        Csv.Decode.fail "Failed to parse"
            )


villachToUdine : OTimetable
villachToUdine =
    [ { from = "Villach Hbf"
      , to = "Udine"
      , links =
            [ { from = fromStringUnsafe "16:49"
              , to = fromStringUnsafe "18:16"
              }
            , { from = fromStringUnsafe "19:29"
              , to = fromStringUnsafe "21:13"
              }
            ]
      }
    , { from = "München Hbf"
      , to = "Villach Hbf"
      , links =
            [ { from = fromStringUnsafe "12:17"
              , to = fromStringUnsafe "16:44"
              }
            , { from = fromStringUnsafe "14:17"
              , to = fromStringUnsafe "18:44"
              }
            ]
      }
    ]


fromStringUnsafe : String -> Time.Posix
fromStringUnsafe hourAndMinutes =
    case Iso8601.toTime <| "2024-07-09T" ++ hourAndMinutes ++ ":00" of
        Ok t ->
            t

        Err _ ->
            Debug.todo <| "Failed to parse " ++ hourAndMinutes
