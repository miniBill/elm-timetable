module SQLite exposing (Database, close, open, serialize, withDb)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import SQLite.Statement as Statement


type Database
    = Database Json.Decode.Value


withDb :
    String
    ->
        (Database
         -> BackendTask { fatal : FatalError, recoverable : BackendTask.Custom.Error } a
        )
    -> BackendTask { fatal : FatalError, recoverable : BackendTask.Custom.Error } a
withDb name op =
    Do.do (open name) <| \db ->
    Do.do (op db) <| \res ->
    Do.do (close db) <| \_ ->
    BackendTask.succeed res


open : String -> BackendTask { fatal : FatalError, recoverable : BackendTask.Custom.Error } Database
open name =
    BackendTask.Custom.run "sqlite_open"
        (Json.Encode.string name)
        (Json.Decode.map Database Json.Decode.value)


close : Database -> BackendTask { fatal : FatalError, recoverable : BackendTask.Custom.Error } ()
close (Database db) =
    BackendTask.Custom.run "sqlite_close"
        db
        (Json.Decode.succeed ())


serialize :
    Database
    -> List Statement.Statement
    -> BackendTask { fatal : FatalError, recoverable : BackendTask.Custom.Error } ()
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
