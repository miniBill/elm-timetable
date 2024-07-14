module Theme exposing (button, column, padding, row, spacing)

import Ui exposing (Attribute, Element)
import Ui.Input


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
        (Ui.Input.button onPress :: padding :: Ui.border 1 :: attrs)
        label
