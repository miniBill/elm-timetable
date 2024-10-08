module Theme exposing (button, column, padding, rhythm, row, spacing, table, tableColumn)

import Color
import Ui exposing (Attribute, Element, shrink, width)
import Ui.Input
import Ui.Table


padding : Attribute msg
padding =
    Ui.padding rhythm


spacing : Attribute msg
spacing =
    Ui.spacing rhythm


rhythm : number
rhythm =
    8


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs children =
    Ui.column (spacing :: attrs) children


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs children =
    Ui.row (spacing :: attrs) children


button :
    List (Attribute msg)
    ->
        { onPress : msg
        , label : Element msg
        }
    -> Element msg
button attrs { onPress, label } =
    Ui.el
        (Ui.Input.button onPress :: padding :: Ui.border 1 :: width shrink :: attrs)
        label


table :
    List (Attribute msg)
    -> List (Maybe (Ui.Table.Column () Int a msg))
    -> List a
    -> Element msg
table attrs columns data =
    let
        count : Int
        count =
            List.length data

        limit : number
        limit =
            100

        tableConfig : Ui.Table.Config () Int a msg
        tableConfig =
            columns
                |> List.filterMap identity
                |> Ui.Table.columns
                |> Ui.Table.withScrollable { stickFirstColumn = True }
                |> Ui.Table.withRowState (\_ index _ -> Just index)
                |> Ui.Table.withRowAttributes
                    (\maybeIndex _ ->
                        case modBy 2 (Maybe.withDefault 0 maybeIndex) of
                            0 ->
                                [ Ui.background Color.grey ]

                            _ ->
                                []
                    )
    in
    if count < limit then
        Ui.Table.viewWithState (Ui.border 1 :: Ui.padding 0 :: attrs)
            tableConfig
            ()
            data

    else
        column []
            [ Ui.Table.viewWithState (Ui.border 1 :: Ui.padding 0 :: attrs)
                tableConfig
                ()
                (List.take limit data)
            , Ui.text <| "Showing " ++ String.fromInt limit ++ " out of " ++ String.fromInt count ++ " results"
            ]


tableColumn :
    String
    -> (item -> value)
    -> (value -> Ui.Table.Cell msg)
    -> Maybe (Ui.Table.Column globalState rowState item msg)
tableColumn header prop viewItem =
    Ui.Table.column
        { header = Ui.Table.header header
        , view = \value -> viewItem (prop value)
        }
        |> Just
