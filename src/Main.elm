module Main exposing (main)

import Browser
import Color
import Csv.Decode
import Data
import Dict exposing (Dict)
import Duration exposing (Duration)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import List.Extra
import Quantity
import RemoteData
import Set
import Time
import TypedSvg exposing (g, line, svg, text_, title)
import TypedSvg.Attributes exposing (class, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..), Paint(..), Transform(..))
import Types exposing (Feed, LocationType(..), Model, Msg(..), OEvent(..), OStation, OViewMode(..), Stop)
import Url exposing (Url)
import Url.Builder


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { timetable =
            if True then
                Data.villachToUdine

            else
                Data.munchenToZoetermeer
      , mode = ViewSimple
      , stops = RemoteData.Loading
      }
    , loadStops
    )


loadStops : Cmd Msg
loadStops =
    Data.feeds
        |> List.map
            (\feed ->
                getCSV (GotStops feed) feed "stops.txt" stopsDecoder
            )
        |> Cmd.batch


getCSV : (Result Http.Error (List a) -> msg) -> String -> String -> Csv.Decode.Decoder a -> Cmd msg
getCSV toMsg feed filename decoder =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.Builder.absolute [ "feeds", feed, filename ] []
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        , expect =
            Http.expectString
                (\got ->
                    got
                        |> Result.andThen
                            (\res ->
                                Csv.Decode.decodeCsv Csv.Decode.FieldNamesFromFirstRow decoder res
                                    |> Result.mapError
                                        (\err ->
                                            case err of
                                                Csv.Decode.DecodingErrors decodingErrs ->
                                                    Http.BadBody (Debug.toString (List.take 10 decodingErrs))

                                                _ ->
                                                    Http.BadBody (Debug.toString err)
                                        )
                            )
                        |> toMsg
                )
        }


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
        (Csv.Decode.string
            |> Csv.Decode.optionalField name
            |> Csv.Decode.andThen
                (\orig ->
                    case orig of
                        Nothing ->
                            Csv.Decode.succeed Nothing

                        Just s ->
                            if String.isEmpty s then
                                Csv.Decode.succeed Nothing

                            else
                                Csv.Decode.map Just decoder
                )
        )
        original


view : Model -> Html Msg
view model =
    Html.div []
        [ case model.mode of
            ViewSimple ->
                viewSimple model
        , Html.button
            [ Html.Events.onClick Reload ]
            [ Html.text "Reload" ]
        , Html.div
            [ Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "padding" "8px"
            ]
          <|
            case model.stops of
                RemoteData.Error e ->
                    [ Html.text (Debug.toString e) ]

                RemoteData.NotAsked ->
                    [ Html.text "Not asked" ]

                RemoteData.Loading ->
                    [ Html.text "Loading..." ]

                RemoteData.Loaded stops ->
                    List.map viewStops (Dict.toList stops)
        ]


viewStops : ( Feed, List Stop ) -> Html msg
viewStops ( feed, stops ) =
    let
        viewStop : Stop -> Html msg
        viewStop stop =
            Html.text (Debug.toString stop)
    in
    Html.div []
        [ Html.text feed
        , stops
            |> List.take 20
            |> List.map viewStop
            |> Html.div
                [ Html.Attributes.style "border" "1px solid black"
                , Html.Attributes.style "padding" "8px"
                ]
        ]


viewSimple : Model -> Html msg
viewSimple model =
    let
        fullHeight : Float
        fullHeight =
            timesHeight * 2 + lineHeight * toFloat (Dict.size stations - 1)

        liftTime :
            (Int -> Int -> Int)
            -> Maybe Time.Posix
            -> Time.Posix
            -> Time.Posix
        liftTime op acc e =
            case acc of
                Nothing ->
                    e

                Just v ->
                    op
                        (Time.posixToMillis v)
                        (Time.posixToMillis e)
                        |> Time.millisToPosix

        addStation :
            OStation
            -> Time.Posix
            -> OEvent
            ->
                Dict
                    OStation
                    { min : Time.Posix
                    , max : Time.Posix
                    , events : Dict Int OEvent
                    }
            ->
                Dict
                    OStation
                    { min : Time.Posix
                    , max : Time.Posix
                    , events : Dict Int OEvent
                    }
        addStation station time event dict =
            let
                new =
                    case Dict.get station dict of
                        Nothing ->
                            { min = time
                            , max = time
                            , events =
                                Dict.singleton (Time.posixToMillis time) event
                            }

                        Just existing ->
                            { min = liftTime min (Just existing.min) time
                            , max = liftTime max (Just existing.max) time
                            , events =
                                Dict.insert (Time.posixToMillis time) event existing.events
                            }
            in
            Dict.insert station new dict

        times : List ( Time.Posix, Time.Posix )
        times =
            model.timetable
                |> List.concatMap
                    (\{ links } ->
                        List.map
                            (\{ from, to } ->
                                ( from, to )
                            )
                            links
                    )

        timeRange : { minTime : Maybe Time.Posix, maxTime : Maybe Time.Posix }
        timeRange =
            List.foldl
                (\( from, to ) acc ->
                    { minTime =
                        Just <| liftTime min acc.minTime from
                    , maxTime =
                        Just <| liftTime max acc.maxTime to
                    }
                )
                { minTime = Nothing
                , maxTime = Nothing
                }
                times

        stations :
            Dict
                OStation
                { min : Time.Posix
                , max : Time.Posix
                , events : Dict Int OEvent
                }
        stations =
            model.timetable
                |> List.foldl
                    (\{ from, to, links } acc ->
                        links
                            |> List.foldl
                                (\link iacc ->
                                    iacc
                                        |> addStation from link.from Departure
                                        |> addStation to link.to Arrival
                                )
                                acc
                    )
                    Dict.empty

        sortedStations :
            List
                ( OStation
                , { events : Dict Int OEvent
                  , min : Time.Posix
                  , max : Time.Posix
                  }
                )
        sortedStations =
            stations
                |> Dict.toList
                |> List.sortBy
                    (\( _, { min, max } ) ->
                        ( Time.posixToMillis min, -(Time.posixToMillis max) )
                    )

        stationPositions : Dict OStation Int
        stationPositions =
            sortedStations
                |> List.indexedMap
                    (\i ( name, _ ) -> ( name, timesHeight + i * lineHeight ))
                |> Dict.fromList

        stationsViews : List (Svg msg)
        stationsViews =
            List.map
                (viewStation timeRange stationPositions)
                sortedStations

        stationToY : OStation -> Float
        stationToY station =
            Dict.get station stationPositions
                |> Maybe.withDefault -1
                |> toFloat

        linksViews : List (Svg msg)
        linksViews =
            model.timetable
                |> List.concatMap
                    (\{ from, to, links } ->
                        links
                            |> List.map
                                (\link ->
                                    line
                                        [ class [ "link" ]
                                        , x1 <| timeToX timeRange link.from
                                        , x2 <| timeToX timeRange link.to
                                        , y1 <| stationToY from
                                        , y2 <| stationToY to
                                        ]
                                        []
                                )
                    )

        timesViews : List (Svg msg)
        timesViews =
            times
                |> List.concatMap
                    (\( from, to ) ->
                        [ Time.posixToMillis from
                        , Time.posixToMillis to
                        ]
                    )
                |> Set.fromList
                |> Set.toList
                |> List.foldr
                    (\t ( last, acc ) ->
                        let
                            time : Time.Posix
                            time =
                                Time.millisToPosix t

                            timeX : Float
                            timeX =
                                timeToX timeRange time

                            vline : Svg msg
                            vline =
                                line
                                    [ class [ "grid" ]
                                    , x1 0
                                    , x2 0
                                    , y1 (timesHeight - pushUp)
                                    , y2 (fullHeight - timesHeight + pushUp)
                                    ]
                                    []

                            label : List (Svg msg)
                            label =
                                let
                                    inner anchor transformation =
                                        text_
                                            [ textAnchor anchor
                                            , transform
                                                [ Translate 5 transformation
                                                , Rotate 90 0 0
                                                ]
                                            ]
                                            [ [ Time.toHour Time.utc time
                                                    |> String.fromInt
                                                    |> String.padLeft 2 ' '
                                              , Time.toMinute Time.utc time
                                                    |> String.fromInt
                                                    |> String.padLeft 2 '0'
                                              ]
                                                |> String.join ":"
                                                |> text
                                            ]
                                in
                                [ inner AnchorStart (timesHeight - pushUp)
                                , inner AnchorEnd (fullHeight - timesHeight + pushUp)
                                ]

                            pushUp =
                                case last of
                                    Nothing ->
                                        timesHeight / 2

                                    Just lastTime ->
                                        if
                                            Duration.from time lastTime
                                                |> Quantity.greaterThan (Duration.minutes 30)
                                        then
                                            timesHeight / 2

                                        else
                                            timesHeight

                            next =
                                g
                                    [ transform [ Translate timeX 0 ] ]
                                    (vline :: label)
                        in
                        ( Just time, next :: acc )
                    )
                    ( Nothing, [] )
                |> Tuple.second

        styleNode =
            TypedSvg.style []
                [ text
                    """
                    .horiz {
                        stroke: black;
                        stroke-width: 2px;
                    }

                    .link {
                        stroke: blue;
                        stroke-width: 2px;
                    }

                    .grid {
                        stroke: gray;
                        stroke-width: 1px;
                        stroke-dasharray: 4;
                    }

                    .wait {
                        stroke-width: 2px;
                    }
                    """
                ]
    in
    svg
        [ Html.Attributes.style "width" "100%"
        , Html.Attributes.style "margin-top" "5vmin"
        , Html.Attributes.style "margin-left" "5vmin"
        , Html.Attributes.style "max-height" "90vh"
        , Html.Attributes.style "max-width" "90vw"
        , viewBox -5 -5 (fullWidth + 10) (fullHeight + 10)
        ]
        (styleNode :: stationsViews ++ linksViews ++ timesViews)


fullWidth : number
fullWidth =
    1000


tableHorizontalMargin : number
tableHorizontalMargin =
    50


lineHeight : number
lineHeight =
    50


timesHeight : number
timesHeight =
    100


namesWidth : number
namesWidth =
    150


timeToX :
    { minTime : Maybe Time.Posix
    , maxTime : Maybe Time.Posix
    }
    -> Time.Posix
    -> Float
timeToX { minTime, maxTime } time =
    case ( minTime, maxTime ) of
        ( Just min, Just max ) ->
            namesWidth
                + tableHorizontalMargin
                + (fullWidth - namesWidth - tableHorizontalMargin * 2)
                * toFloat
                    (Time.posixToMillis time - Time.posixToMillis min)
                / toFloat
                    (Time.posixToMillis max - Time.posixToMillis min)

        _ ->
            -- This never happens but we're going to force a mislayout if the assumptions are wrong
            namesWidth / 2


viewStation :
    { minTime : Maybe Time.Posix, maxTime : Maybe Time.Posix }
    -> Dict OStation Int
    -> ( OStation, { events : Dict Int OEvent, min : Time.Posix, max : Time.Posix } )
    -> Svg msg
viewStation timeRange stationPositions ( name, { events } ) =
    let
        stationY : Float
        stationY =
            Dict.get name stationPositions
                |> Maybe.withDefault -1
                |> toFloat

        waitLines : List (Svg msg)
        waitLines =
            let
                go queue acc =
                    case queue of
                        [] ->
                            List.reverse acc

                        ( at, _ ) :: tail ->
                            let
                                nextDeparture : Maybe Int
                                nextDeparture =
                                    List.Extra.findMap
                                        (\( dep, kind ) ->
                                            if kind == Departure then
                                                Just dep

                                            else
                                                Nothing
                                        )
                                        tail
                            in
                            case nextDeparture of
                                Nothing ->
                                    List.reverse acc

                                Just dep ->
                                    let
                                        duration =
                                            Duration.milliseconds (toFloat (dep - at))

                                        minString : String
                                        minString =
                                            Duration.inMinutes duration
                                                |> floor
                                                |> String.fromInt
                                    in
                                    go tail
                                        (line
                                            [ class [ "wait" ]
                                            , x1 <| timeToX timeRange (Time.millisToPosix at)
                                            , x2 <| timeToX timeRange (Time.millisToPosix dep)
                                            , y1 stationY
                                            , y2 stationY
                                            , stroke (Paint (waitTimeToColor duration))
                                            ]
                                            [ title []
                                                [ text
                                                    (minString
                                                        ++ " min"
                                                    )
                                                ]
                                            ]
                                            :: acc
                                        )
            in
            go (Dict.toList events) []
    in
    g []
        ([ line
            [ class [ "horiz" ]
            , x1 namesWidth
            , x2 fullWidth
            , y1 stationY
            , y2 stationY
            ]
            []
         , text_
            [ y stationY ]
            [ text name ]
         ]
            ++ waitLines
        )


waitTimeToColor : Duration -> Color.Color
waitTimeToColor f =
    if Quantity.lessThan (Duration.minutes 10) f then
        Color.red

    else if Quantity.lessThan (Duration.minutes 60) f then
        Color.orange

    else
        Color.green


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OViewMode mode ->
            ( { model | mode = mode }, Cmd.none )

        GotStops _ (Err e) ->
            ( { model | stops = RemoteData.Error e }, Cmd.none )

        GotStops feed (Ok res) ->
            let
                existing : Dict Feed (List Stop)
                existing =
                    case model.stops of
                        RemoteData.Loaded stops ->
                            stops

                        _ ->
                            Dict.empty
            in
            ( { model | stops = RemoteData.Loaded (Dict.insert feed res existing) }, Cmd.none )

        Reload ->
            ( model, loadStops )


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none
