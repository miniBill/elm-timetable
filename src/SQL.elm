module SQL exposing (main)

import GTFS.Tables
import GTFS.ToSQL as SQLSource
import Html exposing (Html)
import Html.Attributes
import SQLite.Statement as Statement


main : Html msg
main =
    [ SQLSource.toCreate { ifNotExists = False } GTFS.Tables.agency
    , SQLSource.toCreate { ifNotExists = False } GTFS.Tables.stops
    , SQLSource.toCreate { ifNotExists = False } GTFS.Tables.routes
    , SQLSource.toCreate { ifNotExists = False } GTFS.Tables.trips
    , SQLSource.toCreate { ifNotExists = False } GTFS.Tables.stopTimes
    , SQLSource.toCreate { ifNotExists = False } GTFS.Tables.calendars
    , SQLSource.toCreate { ifNotExists = False } GTFS.Tables.calendarDates
    , SQLSource.toCreate { ifNotExists = False } GTFS.Tables.shapePoints
    , SQLSource.toCreate { ifNotExists = False } GTFS.Tables.pathways
    , SQLSource.toCreate { ifNotExists = False } GTFS.Tables.levels
    ]
        |> List.map (\statement -> statement |> Statement.toString)
        |> String.join "\n\n"
        |> Html.text
        |> List.singleton
        |> Html.pre
            [ Html.Attributes.style "font-size" "2rem" ]
