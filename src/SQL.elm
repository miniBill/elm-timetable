module SQL exposing (main)

import GTFS.Tables
import GTFS.ToSQL as SQLSource
import Html exposing (Html)
import Html.Attributes
import SQLite.Statement as Statement


main : Html msg
main =
    SQLSource.toCreate { ifNotExists = False } GTFS.Tables.stops
        |> Statement.toString
        |> Html.text
        |> List.singleton
        |> Html.pre
            [ Html.Attributes.style "font-size" "2rem" ]
