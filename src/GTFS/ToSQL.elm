module GTFS.ToSQL exposing (toCreate)

import SQLite.Statement as Statement
import SQLite.Statement.CreateTable as CreateTable
import SQLite.TableBuilder exposing (Table)
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
toTableDefinition { columns, primaryKey, foreignKeys } =
    let
        primaryKeyConstraint : CreateTable.TableConstraint
        primaryKeyConstraint =
            CreateTable.UnnamedTableConstraint
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
    in
    CreateTable.TableDefinitionColumns
        { options =
            { strict = True
            , withoutRowid = False
            }
        , columns = feedColumn :: columns
        , constraints =
            primaryKeyConstraint :: List.map foreignKeyToConstraint foreignKeys
        }


foreignKeyToConstraint : SQLite.TableBuilder.ForeignKey -> CreateTable.TableConstraint
foreignKeyToConstraint { columnName, tableName, mapsTo } =
    CreateTable.UnnamedTableConstraint
        (CreateTable.TableForeignKey
            [ columnName ]
            { foreignTable = tableName
            , columnNames =
                case mapsTo of
                    Nothing ->
                        []

                    Just mt ->
                        [ mt ]
            , onDelete = Nothing
            , onUpdate = Nothing
            , match = Nothing
            , defer = Nothing
            }
        )


feedColumn : CreateTable.ColumnDefinition
feedColumn =
    { name = "feed"
    , tipe = Just Types.Text
    , constraints =
        [ CreateTable.UnnamedColumnConstraint (CreateTable.ColumnNotNull Nothing) ]
    }
