module SQL exposing (main)

import GTFS.Tables
import GTFS.ToSQL as SQLSource
import Html exposing (Html)
import Html.Attributes
import SQLite.Statement as Statement


main : Html msg
main =
    [ SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.agency)
    , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.stops)
    , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.routes)
    , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.trips)
    , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.stopTimes)
    , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.calendars)
    , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.calendarDates)
    , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.areas)
    , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.stopAreas)
    , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.networks)
    , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.routeNetworks)
    , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.shapePoints)

    -- , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.frequencies)
    -- , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.transfers)
    , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.pathways)
    , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.levels)
    , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.locationGroups)

    -- , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.locationGroupStops)
    -- , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.translations)
    -- , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.feedInfo)
    -- , SQLSource.toCreate { ifNotExists = False } (GTFS.Tables.withFeedColumn GTFS.Tables.attributions)
    ]
        |> List.map (\statement -> statement |> Statement.toString)
        |> String.join "\n\n"
        |> Html.text
        |> List.singleton
        |> Html.pre
            [ Html.Attributes.style "font-size" "1rem" ]
