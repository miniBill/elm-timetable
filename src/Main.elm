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
import Length
import List.Extra
import Maybe.Extra
import Quantity
import RemoteData
import Set exposing (Set)
import Time
import TypedSvg exposing (g, line, svg, text_, title)
import TypedSvg.Attributes exposing (class, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..), Paint(..), Transform(..))
import Types exposing (Feed, Id, LocationType(..), Model, Msg(..), OEvent(..), OStation, OViewMode(..), Pathway, PathwayMode(..), Stop)
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
      , pathways = RemoteData.Loading
      }
    , loadData
    )


loadData : Cmd Msg
loadData =
    Data.feeds
        |> List.concatMap
            (\feed ->
                [ getCSVId GotStops feed "stops.txt" stopsDecoder
                , getCSVId GotPathways feed "pathways.txt" pathwayDecoder
                ]
            )
        |> Cmd.batch


getCSVId :
    (String -> Result Http.Error (Dict Id { a | id : Id }) -> msg)
    -> String
    -> String
    -> Csv.Decode.Decoder { a | id : Id }
    -> Cmd msg
getCSVId toMsg feed filename decoder =
    getCSV
        (\raw ->
            raw
                |> Result.map toDictFromId
                |> toMsg feed
        )
        feed
        filename
        decoder


toDictFromId :
    List { a | id : Id }
    -> Dict Id { a | id : Id }
toDictFromId list =
    List.foldl
        (\stop acc -> Dict.insert stop.id stop acc)
        Dict.empty
        list


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
        (decoder
            |> Csv.Decode.blank
            |> Csv.Decode.optionalField name
            |> Csv.Decode.map Maybe.Extra.join
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
                    case model.pathways of
                        RemoteData.Error e ->
                            [ Html.text (Debug.toString e) ]

                        RemoteData.NotAsked ->
                            [ Html.text "Not asked" ]

                        RemoteData.Loading ->
                            [ Html.text "Loading..." ]

                        RemoteData.Loaded pathways ->
                            Dict.merge
                                (\_ _ acc -> acc)
                                (\k stop pathway acc ->
                                    Dict.insert
                                        k
                                        ( stop, pathway )
                                        acc
                                )
                                (\_ _ acc -> acc)
                                stops
                                pathways
                                Dict.empty
                                |> Dict.toList
                                |> List.map viewFeed
        ]


viewFeed : ( Feed, ( Dict Id Stop, Dict Id Pathway ) ) -> Html msg
viewFeed ( feed, ( stops, pathways ) ) =
    let
        filteredStops : List Stop
        filteredStops =
            stops
                |> Dict.values
                |> List.filter
                    (\stop ->
                        (Just "München Hbf" == stop.name)
                            -- String.contains "Isartor" defaulted
                            || List.member stop.id
                                [ "de:09162:6:40:81"
                                , "de:09162:6_G"
                                ]
                            || (stop.parent_station == Just "de:09162:6_G")
                    )
                |> List.take 50

        stopIds : Set Id
        stopIds =
            Set.fromList (List.map .id filteredStops)

        filteredPathways : List Pathway
        filteredPathways =
            pathways
                |> Dict.values
                |> List.filter
                    (\walkway ->
                        Set.member walkway.from_stop_id stopIds
                            && Set.member walkway.to_stop_id stopIds
                    )
    in
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "8px"
        ]
        [ Html.text feed
        , viewStops filteredStops
        , viewPathways stops filteredPathways
        ]


viewStops : List Stop -> Html msg
viewStops filteredStops =
    let
        mfloat : Maybe Float -> String
        mfloat x =
            x
                |> Maybe.map String.fromFloat
                |> Maybe.withDefault "---"

        viewStop : Stop -> Html msg
        viewStop stop =
            [ stop.id
            , Maybe.withDefault "---" stop.code
            , Maybe.withDefault "---" stop.name
            , Maybe.withDefault "---" stop.tts_name
            , Maybe.withDefault "---" stop.description
            , mfloat stop.lat
            , mfloat stop.lon
            , Maybe.withDefault "---" stop.zone_id
            , Maybe.withDefault "---" <| Maybe.map Url.toString stop.url
            , Debug.toString stop.location_type
            , Maybe.withDefault "---" stop.parent_station
            , Maybe.withDefault "---" stop.timezone
            , Maybe.withDefault "---" <| Maybe.map Debug.toString stop.wheelchair_boarding
            , Maybe.withDefault "---" stop.level_id
            , Maybe.withDefault "---" stop.platform_code
            ]
                |> List.map (\cell -> Html.td [] [ Html.text cell ])
                |> Html.tr []
    in
    filteredStops
        |> List.map viewStop
        |> (::)
            ([ "id"
             , "code"
             , "name"
             , "tts_name"
             , "description"
             , "lat"
             , "lon"
             , "zone_id"
             , "url"
             , "location_type"
             , "parent_station"
             , "timezone"
             , "wheelchair_boarding"
             , "level_id"
             , "platform_code"
             ]
                |> List.map (\col -> Html.th [] [ Html.text col ])
                |> Html.tr []
            )
        |> Html.table
            [ Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "padding" "8px"
            ]


viewPathways : Dict Id Stop -> List Pathway -> Html msg
viewPathways stops filteredPathways =
    let
        stop : Id -> String
        stop id =
            case Dict.get id stops of
                Nothing ->
                    id

                Just found ->
                    [ found.name
                    , found.description
                    ]
                        |> List.filterMap identity
                        |> String.join " - "

        viewPathway : Pathway -> Html msg
        viewPathway pathway =
            [ pathway.id
            , stop pathway.from_stop_id
            , stop pathway.to_stop_id
            ]
                |> List.map (\cell -> Html.td [] [ Html.text cell ])
                |> Html.tr []
    in
    Html.div []
        [ filteredPathways
            |> List.map viewPathway
            |> (::)
                ([ "id"
                 , "from"
                 , "to"
                 ]
                    |> List.map (\col -> Html.th [] [ Html.text col ])
                    |> Html.tr []
                )
            |> Html.table
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
                existing : Dict Feed (Dict Id Stop)
                existing =
                    case model.stops of
                        RemoteData.Loaded stops ->
                            stops

                        _ ->
                            Dict.empty
            in
            ( { model | stops = RemoteData.Loaded (Dict.insert feed res existing) }, Cmd.none )

        GotPathways _ (Err e) ->
            ( { model | pathways = RemoteData.Error e }, Cmd.none )

        GotPathways feed (Ok res) ->
            let
                existing : Dict Feed (Dict Id Pathway)
                existing =
                    case model.pathways of
                        RemoteData.Loaded pathways ->
                            pathways

                        _ ->
                            Dict.empty
            in
            ( { model | pathways = RemoteData.Loaded (Dict.insert feed res existing) }, Cmd.none )

        Reload ->
            ( model, loadData )


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none
