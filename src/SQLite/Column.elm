module SQLite.Column exposing (notNull, nullable, withForeignKey, withForeignKeyTo)

import Csv.Decode
import Json.Encode
import Maybe.Extra
import SQLite.Statement.CreateTable as CreateTable
import SQLite.Table exposing (Codec, Column)


notNull :
    String
    -> (a -> p)
    -> Codec p
    -> Column a p
notNull name getter ( tipe, encode, decoder ) =
    { definition =
        { name = name
        , tipe = Just tipe
        , constraints = [ { name = Nothing, constraint = CreateTable.ColumnNotNull Nothing } ]
        }
    , encode = \v -> encode (getter v)
    , decoder = Csv.Decode.field name decoder
    }


nullable :
    String
    -> (a -> Maybe p)
    -> Codec p
    -> Column a (Maybe p)
nullable name getter ( tipe, encode, decoder ) =
    { definition =
        { name = name
        , tipe = Just tipe
        , constraints = []
        }
    , encode =
        \v ->
            case getter v of
                Nothing ->
                    Json.Encode.null

                Just w ->
                    encode w
    , decoder =
        decoder
            |> Csv.Decode.blank
            |> Csv.Decode.optionalField name
            |> Csv.Decode.map Maybe.Extra.join
    }


withForeignKeyTo : String -> String -> Column a p -> Column a p
withForeignKeyTo tableName columnName ({ definition } as column) =
    { column
        | definition =
            { definition
                | constraints =
                    { name = Nothing
                    , constraint =
                        CreateTable.ColumnForeignKey
                            { foreignTable = tableName
                            , columnNames = [ columnName ]
                            , onDelete = Nothing
                            , onUpdate = Nothing
                            , match = Nothing
                            , defer = Nothing
                            }
                    }
                        :: definition.constraints
            }
    }


withForeignKey : String -> Column a p -> Column a p
withForeignKey tableName ({ definition } as column) =
    { column
        | definition =
            { definition
                | constraints =
                    { name = Nothing
                    , constraint =
                        CreateTable.ColumnForeignKey
                            { foreignTable = tableName
                            , columnNames = []
                            , onDelete = Nothing
                            , onUpdate = Nothing
                            , match = Nothing
                            , defer = Nothing
                            }
                    }
                        :: definition.constraints
            }
    }
