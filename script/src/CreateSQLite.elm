module CreateSQLite exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import GTFS.Tables
import Pages.Script as Script exposing (Script)
import SQLite


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Do.allowFatal
        (SQLite.withDb "../feeds.sqlite"
            (\db ->
                SQLite.serialize db
                    (GTFS.Tables.allCreates True
                        ++ GTFS.Tables.allCreates False
                    )
            )
        )
    <| \_ ->
    Script.log "Done"
