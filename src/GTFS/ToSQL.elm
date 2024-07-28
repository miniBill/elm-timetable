module GTFS.ToSQL exposing (toCreate)

import List.Extra
import SQLite.Statement as Statement
import SQLite.Statement.CreateTable as CreateTable
import SQLite.Table exposing (Table)
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
            { name = Nothing
            , constraint =
                CreateTable.TablePrimaryKey
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
            }

        ( columnForeignKeys, columnsWithoutForeignKeys ) =
            columns
                |> List.map
                    (\column ->
                        case
                            List.Extra.findMap
                                (\{ constraint } ->
                                    case constraint of
                                        CreateTable.ColumnForeignKey fk ->
                                            Just fk

                                        _ ->
                                            Nothing
                                )
                                column.constraints
                        of
                            Just fk ->
                                ( Just ( column.name, fk )
                                , { column
                                    | constraints =
                                        List.Extra.removeWhen
                                            (\{ constraint } -> constraint == CreateTable.ColumnForeignKey fk)
                                            column.constraints
                                  }
                                )

                            Nothing ->
                                ( Nothing, column )
                    )
                |> List.unzip
    in
    CreateTable.TableDefinitionColumns
        { options =
            { strict = True
            , withoutRowid = False
            }
        , columns = feedColumn :: columnsWithoutForeignKeys
        , constraints =
            primaryKeyConstraint
                :: List.filterMap
                    (Maybe.map
                        (\( name, foreignKeyClause ) ->
                            { name = Nothing
                            , constraint =
                                CreateTable.TableForeignKey
                                    [ "feed", name ]
                                    { foreignKeyClause
                                        | columnNames =
                                            if List.isEmpty foreignKeyClause.columnNames then
                                                [ "feed", name ]

                                            else
                                                "feed" :: foreignKeyClause.columnNames
                                    }
                            }
                        )
                    )
                    columnForeignKeys
                ++ List.map foreignKeyToConstraint foreignKeys
        }


foreignKeyToConstraint : SQLite.TableBuilder.ForeignKey -> CreateTable.TableConstraint
foreignKeyToConstraint { columnName, tableName, mapsTo } =
    { name = Nothing
    , constraint =
        CreateTable.TableForeignKey
            [ "feed", columnName ]
            { foreignTable = tableName
            , columnNames = [ "feed", Maybe.withDefault columnName mapsTo ]
            , onDelete = Nothing
            , onUpdate = Nothing
            , match = Nothing
            , defer = Nothing
            }
    }


feedColumn : CreateTable.ColumnDefinition
feedColumn =
    { name = "feed"
    , tipe = Just Types.Text
    , constraints =
        [ { name = Nothing, constraint = CreateTable.ColumnNotNull Nothing } ]
    }
