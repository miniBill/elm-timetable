module SQL exposing (main)

import GTFS.Tables
import Html exposing (Html)
import Html.Attributes
import SQLite.Statement as Statement
import Ui
import Ui.Font


main : Html msg
main =
    GTFS.Tables.allCreates True
        |> List.map (\statement -> statement |> Statement.toString)
        |> String.join "\n\n"
        |> Ui.text
        |> Ui.el
            [ Ui.htmlAttribute (Html.Attributes.style "white-space" "pre")
            , Ui.Font.family [ Ui.Font.monospace ]
            , Ui.width Ui.shrink
            ]
        |> Ui.embed
            [ Ui.padding 8
            , Ui.width Ui.shrink
            ]
