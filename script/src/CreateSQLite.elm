module CreateSQLite exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import FatalError exposing (FatalError)
import GTFS.Tables
import Pages.Script as Script exposing (Script)
import Pages.Script.Spinner as Spinner
import SQLite
import SQLite.Table exposing (Table)


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    SQLite.withDb "../feeds.sqlite"
        (\db ->
            let
                loadTable : Table a -> Spinner.Steps FatalError () -> Spinner.Steps FatalError ()
                loadTable table =
                    Spinner.withStep ("Loading data for table " ++ table.name) (\_ -> SQLite.loadTableFromCsv db "../feeds" "micotra-2024" table)
            in
            Spinner.steps
                |> Spinner.withStep "Create tables"
                    (\_ ->
                        SQLite.serialize db
                            GTFS.Tables.allCreates
                    )
                |> loadTable GTFS.Tables.agency
                |> loadTable GTFS.Tables.levels
                |> loadTable GTFS.Tables.stops
                |> loadTable GTFS.Tables.routes
                |> loadTable GTFS.Tables.trips
                |> loadTable GTFS.Tables.stopTimes
                |> loadTable GTFS.Tables.calendars
                |> loadTable GTFS.Tables.calendarDates
                |> loadTable GTFS.Tables.areas
                |> loadTable GTFS.Tables.stopAreas
                |> loadTable GTFS.Tables.networks
                |> loadTable GTFS.Tables.routeNetworks
                |> loadTable GTFS.Tables.shapePoints
                |> loadTable GTFS.Tables.frequencies
                |> loadTable GTFS.Tables.pathways
                |> loadTable GTFS.Tables.locationGroups
                |> Spinner.runSteps
        )
