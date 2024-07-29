module SQLite.Table exposing (Codec, Column, Table, TableBuilder, table, with, withPrimaryKey)

import Csv.Decode
import Json.Encode
import SQLite.Statement.CreateTable exposing (ColumnDefinition, ForeignKeyClause)
import SQLite.Types


type alias TableBuilder a ctor =
    { name : String
    , filename : String
    , columns : List ColumnDefinition
    , encode : a -> List ( String, Json.Encode.Value )
    , decoder : Csv.Decode.Decoder ctor
    }


type alias Table a =
    { name : String
    , filename : String
    , primaryKey : List String
    , columns : List ColumnDefinition
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


table : String -> String -> ctor -> TableBuilder a ctor
table filename name ctor =
    { name = name
    , filename = filename
    , columns = []
    , encode = \_ -> []
    , decoder = Csv.Decode.succeed ctor
    }


with : Column a p -> TableBuilder a (p -> b) -> TableBuilder a b
with column builder =
    { name = builder.name
    , filename = builder.filename
    , columns = column.definition :: builder.columns
    , encode = \v -> ( column.definition.name, column.encode v ) :: builder.encode v
    , decoder =
        builder.decoder
            |> Csv.Decode.pipeline column.decoder
    }


withPrimaryKey : List String -> TableBuilder a a -> Table a
withPrimaryKey primaryKey { name, filename, encode, decoder, columns } =
    { name = name
    , filename = filename
    , columns = List.reverse columns
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
