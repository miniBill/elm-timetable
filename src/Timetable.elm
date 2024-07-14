module Timetable exposing (view, viewDAG, viewGraph)

import Clock exposing (Clock)
import Color
import Dagre.Attributes
import Dict exposing (Dict)
import Duration exposing (Duration, Seconds)
import Graph
import Html exposing (Html)
import Html.Attributes
import Id exposing (Id)
import IdDict exposing (IdDict)
import IdSet
import List.Extra
import Quantity
import QuantityDict exposing (QuantityDict)
import Render
import Render.StandardDrawers
import Render.StandardDrawers.Attributes
import Render.StandardDrawers.Types
import TypedSvg exposing (circle, g, line, style, svg, text_, title)
import TypedSvg.Attributes exposing (class, id, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..))
import Types exposing (Event(..), Station, Timetable)


lineHeight : number
lineHeight =
    50


timesHeight : number
timesHeight =
    50


namesWidth : number
namesWidth =
    let
        _ =
            -- Calculate this from timetable
            Debug.todo
    in
    250


fullWidth : number
fullWidth =
    1000


tableHorizontalMargin : number
tableHorizontalMargin =
    50


view : Timetable -> Svg msg
view timetable =
    let
        fullHeight : Float
        fullHeight =
            timesHeight * 2 + lineHeight * toFloat (Dict.size stations - 1)

        liftTime :
            (Clock -> Clock -> Clock)
            -> Maybe Clock
            -> Clock
            -> Clock
        liftTime op acc e =
            case acc of
                Nothing ->
                    e

                Just v ->
                    op v e

        addStation :
            Station
            -> Clock
            -> Event
            ->
                Dict
                    Station
                    { min : Clock
                    , max : Clock
                    , events : QuantityDict Int Seconds Event
                    }
            ->
                Dict
                    Station
                    { min : Clock
                    , max : Clock
                    , events : QuantityDict Int Seconds Event
                    }
        addStation station time event dict =
            let
                new =
                    case Dict.get station dict of
                        Nothing ->
                            { min = time
                            , max = time
                            , events =
                                QuantityDict.singleton time event
                            }

                        Just existing ->
                            { min = liftTime Quantity.min (Just existing.min) time
                            , max = liftTime Quantity.max (Just existing.max) time
                            , events =
                                QuantityDict.insert time event existing.events
                            }
            in
            Dict.insert station new dict

        times : List ( Clock, Clock )
        times =
            timetable
                |> List.concatMap
                    (\{ links } ->
                        List.map
                            (\{ from, to } ->
                                ( from, to )
                            )
                            links
                    )

        timeRange : { minTime : Maybe Clock, maxTime : Maybe Clock }
        timeRange =
            List.foldl
                (\( from, to ) acc ->
                    { minTime =
                        Just <| liftTime Quantity.min acc.minTime from
                    , maxTime =
                        Just <| liftTime Quantity.max acc.maxTime to
                    }
                )
                { minTime = Nothing
                , maxTime = Nothing
                }
                times

        stations :
            Dict
                Station
                { min : Clock
                , max : Clock
                , events : QuantityDict Int Seconds Event
                }
        stations =
            timetable
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
                ( Station
                , { events : QuantityDict Int Seconds Event
                  , min : Clock
                  , max : Clock
                  }
                )
        sortedStations =
            stations
                |> Dict.toList
                |> List.sortBy (\( station, _ ) -> stationOrder station)
                |> List.reverse

        stationPositions : Dict Station Int
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

        stationToY : Station -> Float
        stationToY station =
            Dict.get station stationPositions
                |> Maybe.withDefault -1
                |> toFloat
    in
    svg
        [ Html.Attributes.style "width" (String.fromInt fullWidth ++ "px")
        , viewBox -5 -5 (fullWidth + 10) (fullHeight + 10)
        ]
        [ styleNode
        , g [ id "Stations" ] stationsViews
        , g [ id "Links" ] (viewLinks timeRange stationToY timetable)
        , g [ id "Time Grid" ] (viewTimeGrid timeRange fullHeight)
        , g [ id "Endpoints" ] (viewEndpoints timeRange stationToY timetable)
        ]


viewLinks : { minTime : Maybe Clock, maxTime : Maybe Clock } -> (Station -> Float) -> Timetable -> List (Svg msg)
viewLinks timeRange stationToY timetable =
    timetable
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
                                [ title [] [ text link.label ]
                                ]
                        )
            )


viewEndpoints :
    { minTime : Maybe Clock, maxTime : Maybe Clock }
    -> (Station -> Float)
    -> Timetable
    -> List (Svg msg)
viewEndpoints timeRange stationToY timetable =
    timetable
        |> List.concatMap
            (\{ from, to, links } ->
                links
                    |> List.concatMap
                        (\link ->
                            [ circle
                                [ class [ "endpoint" ]
                                , cx <| timeToX timeRange link.from
                                , cy <| stationToY from
                                ]
                                [ title [] [ text (Clock.toHumanString link.from) ] ]
                            , circle
                                [ class [ "endpoint" ]
                                , cx <| timeToX timeRange link.to
                                , cy <| stationToY to
                                ]
                                [ title [] [ text (Clock.toHumanString link.to) ] ]
                            ]
                        )
            )


viewTimeGrid : { minTime : Maybe Clock, maxTime : Maybe Clock } -> Float -> List (Svg msg)
viewTimeGrid timeRange fullHeight =
    case ( timeRange.minTime, timeRange.maxTime ) of
        ( Just minTime, Just maxTime ) ->
            let
                from : Int
                from =
                    floor <| Duration.inHours <| Quantity.toFloatQuantity minTime

                to : Int
                to =
                    ceiling <| Duration.inHours <| Quantity.toFloatQuantity maxTime
            in
            List.range from to
                |> List.map
                    (\hour ->
                        let
                            time : Clock
                            time =
                                Clock.fromHoursMinutesSeconds hour 0 0

                            timeX : Float
                            timeX =
                                timeToX timeRange time

                            verticalLine : Svg msg
                            verticalLine =
                                line
                                    [ class [ "grid" ]
                                    , x1 0
                                    , x2 0
                                    , y1 0
                                    , y2 fullHeight
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
                                            [ Clock.toHumanString time
                                                |> text
                                            ]
                                in
                                [ inner AnchorStart 0
                                , inner AnchorEnd fullHeight
                                ]
                        in
                        g
                            [ id <| String.fromInt hour ++ ":00"
                            , transform [ Translate timeX 0 ]
                            ]
                            (verticalLine :: label)
                    )

        _ ->
            []


styleNode : Svg msg
styleNode =
    style []
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

                    .endpoint {
                        fill: green;
                        r: 5px;
                    }
                    """
        ]


timeToX :
    { minTime : Maybe Clock
    , maxTime : Maybe Clock
    }
    -> Clock
    -> Float
timeToX { minTime, maxTime } time =
    case ( minTime, maxTime ) of
        ( Just min, Just max ) ->
            namesWidth
                + tableHorizontalMargin
                + (fullWidth - namesWidth - tableHorizontalMargin * 2)
                * Quantity.ratio
                    (Quantity.toFloatQuantity <| Quantity.difference time min)
                    (Quantity.toFloatQuantity <| Quantity.difference max min)

        _ ->
            -- This never happens but we're going to force a mislayout if the assumptions are wrong
            namesWidth / 2


viewStation :
    { minTime : Maybe Clock, maxTime : Maybe Clock }
    -> Dict Station Int
    -> ( Station, { events : QuantityDict Int Seconds Event, min : Clock, max : Clock } )
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
                go : List ( Clock, Event ) -> List (Svg msg) -> List (Svg msg)
                go queue acc =
                    case queue of
                        [] ->
                            List.reverse acc

                        ( at, _ ) :: tail ->
                            let
                                nextDeparture : Maybe Clock
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
                                        duration : Duration
                                        duration =
                                            Clock.duration dep at

                                        timeString : String
                                        timeString =
                                            let
                                                rawMins : Int
                                                rawMins =
                                                    Duration.inMinutes duration
                                                        |> floor
                                            in
                                            if rawMins > 60 then
                                                String.fromInt (rawMins // 60)
                                                    ++ "h "
                                                    ++ String.padLeft 2 '0' (String.fromInt (modBy 60 rawMins))
                                                    ++ "m"

                                            else
                                                String.fromInt rawMins ++ "m"
                                    in
                                    go tail
                                        (line
                                            [ class [ "wait" ]
                                            , x1 <| timeToX timeRange at
                                            , x2 <| timeToX timeRange dep
                                            , y1 stationY
                                            , y2 stationY
                                            , stroke (Paint (waitTimeToColor duration))
                                            ]
                                            [ title []
                                                [ text timeString
                                                ]
                                            ]
                                            :: acc
                                        )
            in
            go (QuantityDict.toList events) []
    in
    g [ id <| "Station - " ++ name ]
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


stationOrder : Station -> Int
stationOrder station =
    case station of
        "Trieste Centrale" ->
            1

        "Monfalcone" ->
            2

        "Trieste Airport" ->
            3

        "Cervignano - Aquileia - Grado" ->
            4

        "Palmanova" ->
            5

        "Udine" ->
            6

        "Udine stazione" ->
            6

        "Tarvisio Citta Boscoverde" ->
            12

        "Tarvisio Boscoverde" ->
            12

        "Villach Hauptbahnhof" ->
            18

        "Salzburg Hauptbahnhof" ->
            19

        "Freilassing Bahnhof" ->
            20

        "München Hauptbahnhof" ->
            21

        _ ->
            999


viewGraph : Timetable -> Html msg
viewGraph timetable =
    timetable
        |> List.filterMap
            (\{ from, to, links } ->
                if List.isEmpty links then
                    Nothing

                else
                    Just ( Id.fromString from, Id.fromString to )
            )
        |> viewDAG Id.toString


viewDAG :
    (Id kind -> String)
    -> List ( Id kind, Id kind )
    -> Html msg
viewDAG toName edgeList =
    let
        ids : List (Id kind)
        ids =
            edgeList
                |> List.concatMap (\( from, to ) -> [ from, to ])
                |> IdSet.fromList
                |> IdSet.toList

        idToNodeId : IdDict kind Int
        idToNodeId =
            ids
                |> List.indexedMap (\i id -> ( id, i ))
                |> IdDict.fromList

        edges : List (Graph.Edge ())
        edges =
            edgeList
                |> List.filterMap
                    (\( from, to ) ->
                        Maybe.map2
                            (\fromId toId ->
                                { from = fromId
                                , to = toId
                                , label = ()
                                }
                            )
                            (IdDict.get from idToNodeId)
                            (IdDict.get to idToNodeId)
                    )

        nodes : List (Graph.Node String)
        nodes =
            ids
                |> List.indexedMap
                    (\i id ->
                        { id = i
                        , label = toName id
                        }
                    )

        graph : Graph.Graph String ()
        graph =
            Graph.fromNodesAndEdges nodes edges
    in
    Render.draw
        [ Dagre.Attributes.rankDir Dagre.Attributes.LR

        -- , Dagre.Attributes.widthDict
        --     (stopIds
        --         |> List.map
        --             (\id ->
        --                 ( Dict.get id stopIdToNodeId
        --                     |> Maybe.withDefault -1
        --                 , (10 * String.length (stop id))
        --                     |> toFloat
        --                 )
        --             )
        --         |> Dict.fromList
        --     )
        , Dagre.Attributes.width 300
        ]
        [ Render.nodeDrawer
            (Render.StandardDrawers.svgDrawNode
                [ Render.StandardDrawers.Attributes.title .label
                , Render.StandardDrawers.Attributes.label .label
                ]
            )
        , Render.edgeDrawer
            (Render.StandardDrawers.svgDrawEdge
                [ Render.StandardDrawers.Attributes.arrowHead
                    Render.StandardDrawers.Types.Vee
                , Render.StandardDrawers.Attributes.strokeWidth (\_ -> 4)
                ]
            )

        -- , Render.style "width: 100%;max-height:100vh;max-width:100vw"
        ]
        graph
