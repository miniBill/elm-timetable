module ViaggiaTreno exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Http as Http
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
    Script.log ("Got " ++ String.fromInt (Set.size fromAutocomplete) ++ " train stations from autocomplete")


stationIdsFromAutocomplete : BackendTask { fatal : FatalError, recoverable : Http.Error } (Set String)
stationIdsFromAutocomplete =
    List.range (Char.toCode 'A') (Char.toCode 'Z')
        |> List.map (\code -> searchLetter (Char.fromCode code))
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

                                [ _, codice ] ->
                                    Just codice

                                _ ->
                                    Debug.todo <| "Invalid row: \"" ++ line ++ "\""
                        )
                    |> Set.fromList
            )


searchLetter : Char -> BackendTask { fatal : FatalError, recoverable : Http.Error } String
searchLetter letter =
    let
        str : String
        str =
            String.fromChar letter
    in
    Viaggiatreno.Api.autocompletaStazioneInput
        { params = { input = str }
        }
        |> BackendTask.quiet
