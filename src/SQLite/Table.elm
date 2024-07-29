module SQLite.Table exposing (Codec, Column, Table, TableBuilder, table, with, withPrimaryKey)

import Csv.Decode
import Json.Encode
import SQLite.Statement.CreateTable exposing (ColumnDefinition, ForeignKeyClause)
import SQLite.Types


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


type alias Column a p =
    { definition : ColumnDefinition
    , encode : a -> Json.Encode.Value
    , decoder : Csv.Decode.Decoder p
    }



-- type alias ForeignKey =
--     { columnNames : List String
--     , tableName : String
--     , mapsTo : Maybe (List String)
--     }


type alias Codec a =
    ( SQLite.Types.Type
    , a -> Json.Encode.Value
    , Csv.Decode.Decoder a
    )


table : String -> String -> ctor -> colsCtor -> TableBuilder a ctor colsCtor
table filename name ctor columns =
    { name = name
    , filename = filename
    , columns = columns
    , columnList = []
    , encode = \_ -> []
    , decoder = Csv.Decode.succeed ctor
    }


with : Column a p -> TableBuilder a (p -> b) (Column a p -> c) -> TableBuilder a b c
with column builder =
    { name = builder.name
    , filename = builder.filename
    , columns = builder.columns column
    , columnList = column.definition :: builder.columnList
    , encode = \v -> ( column.definition.name, column.encode v ) :: builder.encode v
    , decoder =
        builder.decoder
            |> Csv.Decode.pipeline column.decoder
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
