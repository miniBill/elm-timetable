module SQLite.Table exposing (Table, TableBuilder, table, with, withPrimaryKey)

import Csv.Decode
import Json.Encode
import SQLite.Column as Column exposing (InnerColumn)
import SQLite.Statement.CreateTable exposing (ColumnDefinition, ForeignKeyClause)


type alias TableBuilder a ctor colsCtor =
    { name : String
    , filename : String
    , columnList : List ColumnDefinition
    , columns : colsCtor
    , encode : a -> List ( String, Json.Encode.Value )
    , decoder : Csv.Decode.Decoder ctor
    }


type alias Table a cols =
    { name : String
    , filename : String
    , primaryKey : List String
    , columnList : List ColumnDefinition
    , columns : cols
    , encode : a -> List ( String, Json.Encode.Value )
    , decoder : Csv.Decode.Decoder a
    , foreignKeys : List ( List String, ForeignKeyClause )
    }



-- type alias ForeignKey =
--     { columnNames : List String
--     , tableName : String
--     , mapsTo : Maybe (List String)
--     }


table : String -> String -> ctor -> colsCtor -> TableBuilder a ctor colsCtor
table filename name ctor columns =
    { name = name
    , filename = filename
    , columns = columns
    , columnList = []
    , encode = \_ -> []
    , decoder = Csv.Decode.succeed ctor
    }


with : InnerColumn a t p -> TableBuilder a (p -> b) (InnerColumn a t p -> c) -> TableBuilder a b c
with column builder =
    let
        definition : ColumnDefinition
        definition =
            Column.definition column
    in
    { name = builder.name
    , filename = builder.filename
    , columns = builder.columns column
    , columnList = definition :: builder.columnList
    , encode = \v -> ( definition.name, Column.encode column v ) :: builder.encode v
    , decoder =
        builder.decoder
            |> Csv.Decode.pipeline
                (Column.decoder column)
    }


withPrimaryKey : List String -> TableBuilder a a cols -> Table a cols
withPrimaryKey primaryKey { name, filename, encode, decoder, columns, columnList } =
    { name = name
    , filename = filename
    , columns = columns
    , columnList = List.reverse columnList
    , encode = encode
    , decoder = decoder
    , primaryKey = primaryKey
    , foreignKeys = []
    }



-- withForeignKey : ForeignKey -> Table a -> Table a
-- withForeignKey { tableName, columnNames, mapsTo } t =
--     { t
--         | foreignKeys =
--             ( columnNames
--             , { foreignTable = tableName
--               , columnNames = Maybe.withDefault [] mapsTo
--               , onDelete = Nothing
--               , onUpdate = Nothing
--               , match = Nothing
--               , defer = Nothing
--               }
--             )
--                 :: t.foreignKeys
--     }
