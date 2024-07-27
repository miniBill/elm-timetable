module GTFS.ToSQL exposing (toCreate)

import SQLite.Statement as Statement
import SQLite.Statement.CreateTable as CreateTable
import SQLite.TableBuilder exposing (Column, ColumnType(..), Table)
import SQLite.Types as Types



-------------------
-- Feed Encoders --
-------------------


toCreate :
    { ifNotExists : Bool
    }
    -> Table a
    -> Statement.Statement
toCreate config t =
    Statement.CreateTable
        { name = t.name
        , ifNotExists = config.ifNotExists
        , temporary = False
        , schemaName = Nothing
        , definition = toTableDefinition t
        }


toTableDefinition : Table a -> CreateTable.TableDefinition
toTableDefinition { columns, primaryKey } =
    CreateTable.TableDefinitionColumns
        { options =
            { strict = True
            , withoutRowid = False
            }
        , columns = feedColumn :: List.map toSqlColumn columns
        , constraints =
            [ CreateTable.UnnamedTableConstraint
                (CreateTable.TablePrimaryKey
                    (List.map
                        (\name ->
                            { nameOrExpr = CreateTable.IsName name
                            , collate = Nothing
                            , ascDesc = Nothing
                            }
                        )
                        (feedColumn.name :: primaryKey)
                    )
                    Nothing
                )
            ]
        }


feedColumn : CreateTable.ColumnDefinition
feedColumn =
    { name = "feed"
    , tipe = Just Types.Text
    , constraints =
        [ CreateTable.UnnamedColumnConstraint (CreateTable.ColumnNotNull Nothing) ]
    }


toSqlColumn : Column -> CreateTable.ColumnDefinition
toSqlColumn { name, tipe } =
    { name = name
    , tipe = Just (typeToSqlType tipe)
    , constraints =
        case tipe of
            Nullable _ ->
                []

            _ ->
                [ CreateTable.UnnamedColumnConstraint (CreateTable.ColumnNotNull Nothing) ]
    }


typeToSqlType : ColumnType -> Types.Type
typeToSqlType tipe =
    case tipe of
        Nullable x ->
            typeToSqlType x

        Integer ->
            Types.Integer

        Real ->
            Types.Real

        Text ->
            Types.Text
