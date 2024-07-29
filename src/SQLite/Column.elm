module SQLite.Column exposing (Color, Column, InnerColumn, NullableColumn, color, column, decoder, definition, encode, nullable, withForeignKey, withSelfForeignKey)

import Csv.Decode
import Json.Encode
import Maybe.Extra
import SQLite.Codec as Codec exposing (Codec)
import SQLite.Statement.CreateTable as CreateTable exposing (ColumnConstraint, ColumnDefinition, ForeignKeyClause)
import SQLite.Types exposing (Type)


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
    InnerColumn a p p


type alias NullableColumn a p =
    InnerColumn a p (Maybe p)


type InnerColumn a t p
    = Column
        { name : String
        , tipe : Maybe Type
        , constraints : List ColumnConstraint
        , getter : a -> p
        , encode : p -> Json.Encode.Value
        , decoder : Csv.Decode.Decoder p
        , nullable : Bool
        }


column :
    String
    -> (a -> t)
    -> Codec t
    -> Column a t
column columnName getter codec =
    { name = columnName
    , tipe = Just codec.tipe
    , nullable = False
    , constraints = []
    , getter = getter
    , encode = codec.encode
    , decoder =
        codec.decoder
            |> Csv.Decode.field columnName
    }
        |> Column


nullable :
    String
    -> (a -> Maybe t)
    -> Codec t
    -> NullableColumn a t
nullable columnName getter codec =
    { name = columnName
    , tipe = Just codec.tipe
    , nullable = True
    , constraints = []
    , getter = getter
    , encode =
        \v ->
            case v of
                Nothing ->
                    Json.Encode.null

                Just w ->
                    codec.encode w
    , decoder =
        codec.decoder
            |> Csv.Decode.blank
            |> Csv.Decode.optionalField columnName
            |> Csv.Decode.map Maybe.Extra.join
    }
        |> Column


withForeignKey :
    Table a cols
    -> (cols -> InnerColumn a t q)
    -> InnerColumn b t p
    -> InnerColumn b t p
withForeignKey table getColumn (Column c) =
    { c
        | constraints =
            { name = Just ("to_" ++ table.name)
            , constraint =
                CreateTable.ColumnForeignKey
                    { foreignTable = table.name
                    , columnNames = [ name (getColumn table.columns) ]
                    , onDelete = Nothing
                    , onUpdate = Nothing
                    , match = Nothing
                    , defer = Nothing
                    }
            }
                :: c.constraints
    }
        |> Column


withSelfForeignKey : String -> String -> InnerColumn a t p -> InnerColumn a t p
withSelfForeignKey tableName columnName (Column c) =
    Column
        { c
            | constraints =
                { name = Just ("to_" ++ tableName)
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
                    :: c.constraints
        }


type alias Color =
    String


color : Codec Color
color =
    Codec.string


definition : InnerColumn a t p -> ColumnDefinition
definition (Column c) =
    { name = c.name
    , tipe = c.tipe
    , constraints =
        if c.nullable then
            c.constraints

        else
            { name = Nothing, constraint = CreateTable.ColumnNotNull Nothing } :: c.constraints
    }


encode : InnerColumn a t p -> a -> Json.Encode.Value
encode (Column c) v =
    c.encode (c.getter v)


decoder : InnerColumn a t p -> Csv.Decode.Decoder p
decoder (Column c) =
    c.decoder


name : InnerColumn a t p -> String
name (Column c) =
    c.name
