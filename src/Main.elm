module Main exposing (main)

import Browser
import Color
import Dict exposing (Dict)
import Duration exposing (Duration)
import Html exposing (Html)
import Html.Attributes
import Iso8601
import List.Extra
import Quantity
import Set
import Time
import TypedSvg exposing (g, line, svg, text_, title)
import TypedSvg.Attributes exposing (class, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..), Paint(..), Transform(..))


type alias Timetable =
    List
        { from : Spacetime
        , to : Spacetime
        }


type alias Spacetime =
    { station : Station
    , time : Time.Posix
    }


type alias Station =
    String


type Event
    = Arrival
    | Departure


type alias Model =
    { timetable : Timetable
    , mode : ViewMode
    }


type ViewMode
    = ViewSimple


type Msg
    = ViewMode ViewMode


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd msg )
init _ =
    ( { timetable =
            [ { from =
                    { station = "Villach Hbf"
                    , time = fromStringUnsafe "16:49"
                    }
              , to =
                    { station = "Udine"
                    , time = fromStringUnsafe "18:16"
                    }
              }
            , { from =
                    { station = "Villach Hbf"
                    , time = fromStringUnsafe "19:29"
                    }
              , to =
                    { station = "Udine"
                    , time = fromStringUnsafe "21:13"
                    }
              }
            , { from =
                    { station = "München Hbf"
                    , time = fromStringUnsafe "12:17"
                    }
              , to =
                    { station = "Villach Hbf"
                    , time = fromStringUnsafe "16:44"
                    }
              }
            , { from =
                    { station = "München Hbf"
                    , time = fromStringUnsafe "14:17"
                    }
              , to =
                    { station = "Villach Hbf"
                    , time = fromStringUnsafe "18:44"
                    }
              }
            ]
      , mode = ViewSimple
      }
    , Cmd.none
    )


fromStringUnsafe : String -> Time.Posix
fromStringUnsafe hourAndMinutes =
    case Iso8601.toTime <| "2024-07-09T" ++ hourAndMinutes ++ ":00" of
        Ok t ->
            t

        Err _ ->
            Debug.todo <| "Failed to parse " ++ hourAndMinutes


view : Model -> Html Msg
view model =
    case model.mode of
        ViewSimple ->
            viewSimple model


viewSimple : Model -> Html msg
viewSimple model =
    let
        fullWidth : number
        fullWidth =
            500

        namesWidth : number
        namesWidth =
            100

        tableHorizontalMargin : number
        tableHorizontalMargin =
            50

        lineHeight : number
        lineHeight =
            50

        timesHeight : number
        timesHeight =
            100

        fullHeight =
            timesHeight + lineHeight * toFloat (Dict.size stations - 1)

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
            Spacetime
            -> Event
            ->
                Dict
                    Station
                    { min : Time.Posix
                    , max : Time.Posix
                    , events : Dict Int Event
                    }
            ->
                Dict
                    Station
                    { min : Time.Posix
                    , max : Time.Posix
                    , events : Dict Int Event
                    }
        addStation { station, time } event dict =
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

        { stations, minTime, maxTime } =
            model.timetable
                |> List.foldl
                    (\{ from, to } acc ->
                        { minTime =
                            Just <| liftTime min acc.minTime from.time
                        , maxTime =
                            Just <| liftTime max acc.maxTime to.time
                        , stations =
                            acc.stations
                                |> addStation from Departure
                                |> addStation to Arrival
                        }
                    )
                    { minTime = Nothing
                    , maxTime = Nothing
                    , stations = Dict.empty
                    }

        sortedStations =
            stations
                |> Dict.toList
                |> List.sortBy
                    (\( _, { min, max } ) ->
                        ( Time.posixToMillis min, -(Time.posixToMillis max) )
                    )

        stationPositions =
            sortedStations
                |> List.indexedMap
                    (\i ( name, _ ) -> ( name, timesHeight + i * lineHeight ))
                |> Dict.fromList

        stationsViews =
            sortedStations
                |> List.map
                    (\( name, { events } ) ->
                        let
                            stationY =
                                stationToY name

                            waitLines =
                                let
                                    go queue acc =
                                        case queue of
                                            [] ->
                                                List.reverse acc

                                            ( at, _ ) :: tail ->
                                                let
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

                                                            minString =
                                                                Duration.inMinutes duration
                                                                    |> floor
                                                                    |> String.fromInt
                                                        in
                                                        go tail
                                                            (line
                                                                [ class [ "wait" ]
                                                                , x1 <| timeToX (Time.millisToPosix at)
                                                                , x2 <| timeToX (Time.millisToPosix dep)
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
                    )

        timeToX : Time.Posix -> Float
        timeToX time =
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

        stationToY station =
            Dict.get station stationPositions
                |> Maybe.withDefault -1
                |> toFloat

        linksViews =
            model.timetable
                |> List.map
                    (\{ from, to } ->
                        line
                            [ class [ "link" ]
                            , x1 <| timeToX from.time
                            , x2 <| timeToX to.time
                            , y1 <| stationToY from.station
                            , y2 <| stationToY to.station
                            ]
                            []
                    )

        timesViews =
            model.timetable
                |> List.concatMap
                    (\{ from, to } ->
                        [ Time.posixToMillis from.time
                        , Time.posixToMillis to.time
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
                                timeToX time

                            vline : Svg msg
                            vline =
                                line
                                    [ class [ "grid" ]
                                    , x1 0
                                    , x2 0
                                    , y1 (timesHeight - pushUp)
                                    , y2 fullHeight
                                    ]
                                    []

                            label : Svg msg
                            label =
                                text_
                                    [ textAnchor AnchorStart
                                    , transform
                                        [ Translate 5 (timesHeight - pushUp)
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
                                    [ label
                                    , vline
                                    ]
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
                        stroke-width: 1px;
                    }

                    .link {
                        stroke: blue;
                        stroke-width: 1px;
                    }

                    .grid {
                        stroke: gray;
                        stroke-width: 1px;
                        stroke-dasharray: 4;
                    }

                    .wait {
                        stroke-width: 4px;
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


waitTimeToColor : Duration -> Color.Color
waitTimeToColor f =
    if Quantity.lessThan (Duration.minutes 10) f then
        Color.red

    else if Quantity.lessThan (Duration.minutes 60) f then
        Color.orange

    else
        Color.green


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ViewMode mode ->
            ( { model | mode = mode }, Cmd.none )


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none
