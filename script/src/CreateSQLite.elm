module CreateSQLite exposing (run)

import BackendTask exposing (BackendTask)
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
                loadTable : String -> Table a -> Spinner.Steps FatalError () -> Spinner.Steps FatalError ()
                loadTable feed table =
                    Spinner.withStep ("[" ++ feed ++ "] Loading data for table " ++ table.name) (\_ -> SQLite.loadTableFromCsv db "../feeds" feed table)

                loadFeed : String -> Spinner.Steps FatalError () -> Spinner.Steps FatalError ()
                loadFeed feed steps =
                    steps
                        |> loadTable feed GTFS.Tables.agency
                        |> loadTable feed GTFS.Tables.levels
                        |> loadTable feed GTFS.Tables.stops
                        |> loadTable feed GTFS.Tables.routes
                        |> loadTable feed GTFS.Tables.trips
                        |> loadTable feed GTFS.Tables.stopTimes
                        |> loadTable feed GTFS.Tables.calendars
                        |> loadTable feed GTFS.Tables.calendarDates
                        |> loadTable feed GTFS.Tables.areas
                        |> loadTable feed GTFS.Tables.stopAreas
                        |> loadTable feed GTFS.Tables.networks
                        |> loadTable feed GTFS.Tables.routeNetworks
                        -- |> loadTable feed GTFS.Tables.shapePoints
                        |> loadTable feed GTFS.Tables.frequencies
                        |> loadTable feed GTFS.Tables.pathways
                        |> loadTable feed GTFS.Tables.locationGroups
            in
            Spinner.steps
                |> Spinner.withStep "Create tables"
                    (\_ ->
                        SQLite.serialize db
                            GTFS.Tables.allCreates
                    )
                |> loadFeed "micotra-2024"
                |> loadFeed "oebb-2024"
                |> Spinner.runSteps
        )
