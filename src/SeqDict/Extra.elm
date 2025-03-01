module SeqDict.Extra exposing (groupBy)

import SeqDict exposing (SeqDict)


groupBy : (a -> id) -> List a -> SeqDict id (List a)
groupBy f list =
    List.foldl
        (\e acc ->
            let
                key : id
                key =
                    f e
            in
            SeqDict.update key
                (\existing ->
                    existing
                        |> Maybe.withDefault []
                        |> (::) e
                        |> Just
                )
                acc
        )
        SeqDict.empty
        list
