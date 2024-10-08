module GTFS.ToSQL exposing (toCreate)

import SQLite.Statement as Statement
import SQLite.Statement.CreateTable as CreateTable
import SQLite.Table exposing (Table)



-------------------
-- Feed Encoders --
-------------------


toCreate :
    { ifNotExists : Bool
    }
    -> Table a cols
    -> Statement.Statement
toCreate config t =
    { explain = Nothing
    , statement =
        Statement.CreateTable
            { name = t.name
            , ifNotExists = config.ifNotExists
            , temporary = False
            , schemaName = Nothing
            , definition = toTableDefinition t
            }
    }


toTableDefinition : Table a cols -> CreateTable.TableDefinition
toTableDefinition { columnList, primaryKey, foreignKeys } =
    CreateTable.TableDefinitionColumns
        { options =
            { strict = True
            , withoutRowid = False
            }
        , columns = columnList
        , constraints =
            primaryKeyToConstraint primaryKey
                :: List.map foreignKeyToConstraint foreignKeys
        }


primaryKeyToConstraint : List String -> CreateTable.TableConstraint
primaryKeyToConstraint primaryKey =
    { name = Just "pk"
    , constraint =
        CreateTable.TablePrimaryKey
            (List.map
                (\name ->
                    { nameOrExpr = CreateTable.IsName name
                    , collate = Nothing
                    , ascDesc = Nothing
                    }
                )
                primaryKey
            )
            Nothing
    }


foreignKeyToConstraint : ( List String, CreateTable.ForeignKeyClause ) -> CreateTable.TableConstraint
foreignKeyToConstraint ( columnNames, clause ) =
    { name = Just <| "fk_" ++ clause.foreignTable
    , constraint = CreateTable.TableForeignKey columnNames clause
    }
