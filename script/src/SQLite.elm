module SQLite exposing (Database, close, loadTableFromCsv, open, serialize, withDb)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import SQLite.Statement as Statement
import SQLite.Table exposing (Table)


type Database
    = Database Json.Decode.Value


withDb :
    String
    ->
        (Database
         -> BackendTask FatalError a
        )
    -> BackendTask FatalError a
withDb name op =
    Do.do (open name) <| \db ->
    Do.do (op db) <| \res ->
    Do.do (close db) <| \_ ->
    BackendTask.succeed res


open : String -> BackendTask FatalError Database
open name =
    BackendTask.Custom.run "sqlite_open"
        (Json.Encode.string name)
        (Json.Decode.map Database Json.Decode.value)
        |> BackendTask.quiet
        |> BackendTask.allowFatal


close : Database -> BackendTask FatalError ()
close (Database db) =
    BackendTask.Custom.run "sqlite_close"
        db
        (Json.Decode.succeed ())
        |> BackendTask.quiet
        |> BackendTask.allowFatal


serialize :
    Database
    -> List Statement.Statement
    -> BackendTask FatalError ()
serialize (Database db) statements =
    BackendTask.Custom.run "sqlite_serialize"
        (Json.Encode.object
            [ ( "db", db )
            , ( "statements"
              , statements
                    |> Json.Encode.list
                        (\statement ->
                            Json.Encode.object
                                [ ( "statement"
                                  , statement
                                        |> Statement.toString
                                        |> Json.Encode.string
                                  )
                                , ( "params"
                                  , Json.Encode.list never []
                                  )
                                ]
                        )
              )
            ]
        )
        (Json.Decode.succeed ())
        |> BackendTask.quiet
        |> BackendTask.allowFatal


loadTableFromCsv : Database -> String -> String -> Table a -> BackendTask FatalError ()
loadTableFromCsv (Database db) dir feed table =
    BackendTask.Custom.run "sqlite_load_csv"
        (Json.Encode.object
            [ ( "db", db )
            , ( "dir", Json.Encode.string dir )
            , ( "feed", Json.Encode.string feed )
            , ( "filename", Json.Encode.string table.filename )
            , ( "table", Json.Encode.string table.name )
            ]
        )
        (Json.Decode.succeed ())
        |> BackendTask.quiet
        |> BackendTask.allowFatal
