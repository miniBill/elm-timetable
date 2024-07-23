module ViaggiaTreno exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Http as Http
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Set exposing (Set)
import Viaggiatreno.Api


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Do.allowFatal stationIdsFromAutocomplete <| \fromAutocomplete ->
    Do.allowFatal stationIdsFromRegions <| \fromRegions ->
    Script.log ("Got " ++ String.fromInt (Dict.size fromAutocomplete) ++ " train stations from autocomplete, " ++ String.fromInt (Set.size fromRegions) ++ " from regions")


stationIdsFromRegions : BackendTask { fatal : FatalError, recoverable : Http.Error } (Set String)
stationIdsFromRegions =
    List.range 0 22
        |> List.map
            (\idRegione ->
                Viaggiatreno.Api.elencoStazioniIdRegione
                    { params =
                        { idRegione = idRegione }
                    }
                    |> BackendTask.quiet
            )
        |> BackendTask.combine
        |> BackendTask.map
            (\res ->
                res
                    |> List.concat
                    |> List.map
                        (\station ->
                            station.codiceStazione
                        )
                    |> Set.fromList
            )


stationIdsFromAutocomplete : BackendTask { fatal : FatalError, recoverable : Http.Error } (Dict String String)
stationIdsFromAutocomplete =
    List.range (Char.toCode 'A') (Char.toCode 'Z')
        |> List.map
            (\code ->
                Viaggiatreno.Api.autocompletaStazioneInput
                    { params =
                        { input =
                            String.fromChar (Char.fromCode code)
                        }
                    }
                    |> BackendTask.quiet
            )
        |> BackendTask.combine
        |> BackendTask.map
            (\res ->
                res
                    |> List.concatMap (String.split "\n")
                    |> List.filterMap
                        (\line ->
                            case String.split "|" line of
                                [] ->
                                    Nothing

                                [ "" ] ->
                                    Nothing

                                [ nome, codice ] ->
                                    Just ( codice, nome )

                                _ ->
                                    Debug.todo <| "Invalid row: \"" ++ line ++ "\""
                        )
                    |> Dict.fromList
            )
