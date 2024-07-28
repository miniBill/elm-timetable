module SQL exposing (main)

import GTFS.Tables
import GTFS.ToSQL as SQLSource
import Html exposing (Html)
import Html.Attributes
import SQLite.Statement as Statement
import SQLite.Table exposing (Table)
import Ui
import Ui.Font


main : Html msg
main =
    let
        maybeAddFeed : Table a -> Table a
        maybeAddFeed table =
            if False then
                GTFS.Tables.withFeedColumn table

            else
                { table | name = table.name ++ "_without_feed" }
    in
    [ SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.agency)
    , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.stops)
    , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.routes)
    , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.trips)
    , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.stopTimes)
    , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.calendars)
    , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.calendarDates)
    , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.areas)
    , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.stopAreas)
    , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.networks)
    , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.routeNetworks)
    , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.shapePoints)
    , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.frequencies)

    -- , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.transfers)
    , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.pathways)
    , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.levels)
    , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.locationGroups)

    -- , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.locationGroupStops)
    -- , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.translations)
    -- , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.feedInfo)
    -- , SQLSource.toCreate { ifNotExists = False } (maybeAddFeed GTFS.Tables.attributions)
    ]
        |> List.map (\statement -> statement |> Statement.toString)
        |> String.join "\n\n"
        |> Ui.text
        |> Ui.el
            [ Ui.htmlAttribute (Html.Attributes.style "white-space" "pre")
            , Ui.Font.family [ Ui.Font.monospace ]
            , Ui.width Ui.shrink
            ]
        |> Ui.embed
            [ Ui.padding 8
            , Ui.width Ui.shrink
            ]
