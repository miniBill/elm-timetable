module Timetable exposing (view, viewDAG, viewGraph)

import Clock exposing (Clock)
import Color
import Dagre.Attributes
import Dict exposing (Dict)
import Duration exposing (Seconds)
import Graph
import Html exposing (Html)
import Html.Attributes
import Id exposing (Id)
import IdDict exposing (IdDict)
import IdSet
import Quantity
import QuantityDict exposing (QuantityDict)
import Render
import Render.StandardDrawers
import Render.StandardDrawers.Attributes
import Render.StandardDrawers.Types
import Set
import TypedSvg exposing (circle, g, line, style, svg, text_, title)
import TypedSvg.Attributes exposing (class, id, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..))
import Types exposing (Event(..), Station, Timetable)
import Ui


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
    8


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

        stationToY : Station -> Float
        stationToY station =
            Dict.get station stationPositions
                |> Maybe.withDefault -1
                |> toFloat
    in
    case ( timeRange.minTime, timeRange.maxTime ) of
        ( Just minTime, Just maxTime ) ->
            let
                timeRange_ : { minTime : Clock, maxTime : Clock }
                timeRange_ =
                    { minTime =
                        Quantity.multiplyBy
                            (Quantity.ratio
                                (minTime
                                    |> Quantity.toFloatQuantity
                                )
                                (Duration.minutes 15)
                                |> floor
                            )
                            (Clock.fromHoursMinutesSeconds 0 15 0)
                    , maxTime =
                        Quantity.multiplyBy
                            (Quantity.ratio
                                (maxTime
                                    |> Quantity.toFloatQuantity
                                )
                                (Duration.minutes 15)
                                |> ceiling
                            )
                            (Clock.fromHoursMinutesSeconds 0 15 0)
                    }
            in
            svg
                [ Html.Attributes.style "width" (String.fromInt fullWidth ++ "px")
                , viewBox 0 0 fullWidth fullHeight
                ]
                [ styleNode
                , g [ id "Stations" ] (List.map (viewStation timeRange_ stationPositions) sortedStations)
                , g [ id "Links" ] (viewLinks timeRange_ stationToY timetable)
                , g [ id "Time Grid" ] (viewTimeGrid timeRange_ fullHeight)
                , g [ id "Endpoints" ] (viewEndpoints timeRange_ stationToY timetable)
                ]

        _ ->
            Html.text "Empty timetable"


viewLinks : { minTime : Clock, maxTime : Clock } -> (Station -> Float) -> Timetable -> List (Svg msg)
viewLinks timeRange stationToY timetable =
    let
        colorDict : Dict String Color.Color
        colorDict =
            timetable
                |> List.concatMap .links
                |> List.map .train
                |> Set.fromList
                |> Set.toList
                |> pairWithColors
                |> Dict.fromList

        toColor : String -> Color.Color
        toColor train =
            Dict.get train colorDict
                |> Maybe.withDefault Color.blue
    in
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
                                , stroke (Paint (toColor link.train))
                                ]
                                [ title [] [ text link.label ]
                                ]
                        )
            )


pairWithColors : List a -> List ( a, Color.Color )
pairWithColors list =
    let
        colors : List Ui.Color
        colors =
            tab10

        go : List a -> List Color.Color -> List ( a, Color.Color ) -> List ( a, Color.Color )
        go queue cqueue acc =
            case queue of
                [] ->
                    List.reverse acc

                head :: tail ->
                    case cqueue of
                        [] ->
                            go queue colors acc

                        chead :: ctail ->
                            go tail ctail (( head, chead ) :: acc)
    in
    go list colors []


tab10 : List Ui.Color
tab10 =
    [ Ui.rgb 0x04 0x58 0x93
    , Ui.rgb 0xDB 0x61 0x00
    , Ui.rgb 0x10 0x80 0x10
    , Ui.rgb 0xB4 0x0C 0x0D
    , Ui.rgb 0x74 0x49 0x9C
    , Ui.rgb 0x6D 0x39 0x2E
    , Ui.rgb 0xC1 0x58 0xA0
    , Ui.rgb 0x61 0x61 0x61
    , Ui.rgb 0x9A 0x9C 0x07
    , Ui.rgb 0x00 0x9D 0xAE
    ]


viewEndpoints :
    { minTime : Clock, maxTime : Clock }
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


viewTimeGrid : { minTime : Clock, maxTime : Clock } -> Float -> List (Svg msg)
viewTimeGrid ({ minTime, maxTime } as timeRange) fullHeight =
    let
        from : Int
        from =
            floor <| 4 * (Duration.inHours <| Quantity.toFloatQuantity minTime)

        to : Int
        to =
            ceiling <| 4 * (Duration.inHours <| Quantity.toFloatQuantity maxTime)
    in
    List.range from to
        |> List.map
            (\quarterHour ->
                let
                    time : Clock
                    time =
                        Clock.fromHoursMinutesSeconds 0 (quarterHour * 15) 0

                    timeX : Float
                    timeX =
                        timeToX timeRange time

                    children : List (Svg msg)
                    children =
                        if modBy 4 quarterHour == 0 then
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
                            [ line
                                [ class [ "grid" ]
                                , x1 0
                                , x2 0
                                , y1 0
                                , y2 fullHeight
                                ]
                                []
                            , inner AnchorStart 0
                            , inner AnchorEnd fullHeight
                            ]

                        else
                            [ line
                                [ class [ "grid", "secondary" ]
                                , x1 0
                                , x2 0
                                , y1 timesHeight
                                , y2 (fullHeight - timesHeight)
                                ]
                                []
                            ]
                in
                g
                    [ id <| Clock.toHumanString time
                    , transform [ Translate timeX 0 ]
                    ]
                    children
            )


styleNode : Svg msg
styleNode =
    style []
        [ text
            """
            .stationLine {
                stroke: gray;
                stroke-width: 1px;
            }

            .link {
                stroke-width: 2px;
            }

            .grid {
                stroke: gray;
                stroke-width: 1px;
            }

            .secondary {
                stroke-dasharray: 4;
            }

            .endpoint {
                fill: green;
                r: 5px;
            }
            """
        ]


timeToX :
    { minTime : Clock
    , maxTime : Clock
    }
    -> Clock
    -> Float
timeToX { minTime, maxTime } time =
    namesWidth
        + tableHorizontalMargin
        + (fullWidth - namesWidth - tableHorizontalMargin * 2)
        * Quantity.ratio
            (Quantity.toFloatQuantity <| Quantity.difference time minTime)
            (Quantity.toFloatQuantity <| Quantity.difference maxTime minTime)


viewStation :
    { minTime : Clock, maxTime : Clock }
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
    in
    g [ id <| "Station - " ++ name ]
        [ line
            [ class [ "stationLine" ]
            , x1 (timeToX timeRange timeRange.minTime - 1)
            , x2 (timeToX timeRange timeRange.maxTime + 1)
            , y1 stationY
            , y2 stationY
            ]
            []
        , text_
            [ y stationY ]
            [ text name ]
        ]


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
