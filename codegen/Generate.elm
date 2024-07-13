module Generate exposing (main)

{-| -}

import Elm.Annotation as Annotation
import Gen.CodeGen.Generate as Generate
import Gen.Id
import Gen.Quantity
import GenericDict
import GenericSet


main : Program {} () ()
main =
    Generate.run
        [ GenericDict.init
            { keyType = Gen.Id.annotation_.id (Annotation.var "kind")
            , namespace = []
            , toComparable = Gen.Id.toString
            }
            |> GenericDict.useElmFastDict
            |> GenericDict.withTypeName "IdDict"
            |> GenericDict.generateFile
        , GenericSet.init
            { valueType = Gen.Id.annotation_.id (Annotation.var "kind")
            , namespace = []
            , toComparable = Gen.Id.toString
            }
            |> GenericSet.useElmFastDict
            |> GenericSet.withTypeName "IdSet"
            |> GenericSet.generateFile
        , GenericDict.init
            { keyType = Gen.Quantity.annotation_.quantity (Annotation.var "comparable") (Annotation.var "unit")
            , namespace = []
            , toComparable = Gen.Quantity.unwrap
            }
            |> GenericDict.useElmFastDict
            |> GenericDict.withTypeName "QuantityDict"
            |> GenericDict.generateFile
        , GenericSet.init
            { valueType = Gen.Quantity.annotation_.quantity (Annotation.var "comparable") (Annotation.var "unit")
            , namespace = []
            , toComparable = Gen.Quantity.unwrap
            }
            |> GenericSet.useElmFastDict
            |> GenericSet.withTypeName "QuantitySet"
            |> GenericSet.generateFile
        ]
