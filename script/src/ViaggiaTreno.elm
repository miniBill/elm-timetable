module ViaggiaTreno exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Http as Http
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import List.Extra
import Pages.Script as Script exposing (Script)
import Set exposing (Set)
import Viaggiatreno.Api
import Viaggiatreno.Types exposing (StationDetails)


run : Script
run =
    Script.withoutCliOptions script


script : BackendTask FatalError ()
script =
    Do.log "Getting stations from autocomplete" <| \_ ->
    Do.allowFatal stationIdsFromAutocomplete <| \fromAutocomplete ->
    Do.log "Getting stations from regions" <| \_ ->
    Do.allowFatal (stationIdsFromRegions fromAutocomplete) <| \fromRegions ->
    Script.log ("Got " ++ String.fromInt (Dict.size fromAutocomplete) ++ " train stations from autocomplete, " ++ String.fromInt (Dict.size fromRegions) ++ " from regions")


stationIdsFromRegions : Dict String v -> BackendTask { fatal : FatalError, recoverable : Http.Error } (Dict String StationDetails)
stationIdsFromRegions dict =
    let
        ids : List String
        ids =
            Dict.keys dict
    in
    ids
        |> List.map
            (\idStazione ->
                wrapCache
                    (Viaggiatreno.Api.regioneIdStazione
                        { params =
                            { idStazione = idStazione }
                        }
                        |> BackendTask.quiet
                    )
                    |> BackendTask.map Just
                    |> BackendTask.onError (\_ -> BackendTask.succeed Nothing)
            )
        |> List.Extra.greedyGroupsOf 10
        |> List.map BackendTask.combine
        |> BackendTask.sequence
        |> BackendTask.andThen
            (\regionIds ->
                regionIds
                    |> List.concat
                    |> List.filterMap identity
                    |> Set.fromList
                    |> Set.toList
                    |> Debug.log "regionIds"
                    |> List.map
                        (\idRegione ->
                            wrapCache
                                (Viaggiatreno.Api.elencoStazioniIdRegione
                                    { params =
                                        { idRegione = idRegione }
                                    }
                                    |> BackendTask.quiet
                                )
                        )
                    |> BackendTask.combine
                    |> BackendTask.map
                        (\res ->
                            res
                                |> List.concat
                                |> List.map
                                    (\station ->
                                        ( station.codiceStazione, station )
                                    )
                                |> Dict.fromList
                        )
            )


wrapCache :
    BackendTask
        { fatal : FatalError
        , recoverable : Http.Error
        }
        t
    ->
        BackendTask
            { fatal : FatalError
            , recoverable : Http.Error
            }
            t
wrapCache task =
    BackendTask.onError
        (\err ->
            case err.recoverable of
                Http.BadStatus metadata _ ->
                    if metadata.statusCode == 404 then
                        Do.log ("Cache miss " ++ metadata.url) <| \_ ->
                        Do.do (Script.sleep 1000) <| \_ ->
                        Do.do
                            (Http.get
                                (metadata.url
                                    |> String.replace
                                        "http://localhost:9000/cache/"
                                        "http://www.viaggiatreno.it/infomobilita/resteasy/viaggiatreno/"
                                )
                                Http.expectString
                            )
                        <| \raw ->
                        Do.do
                            (Script.writeFile
                                { path =
                                    metadata.url
                                        |> String.replace
                                            "http://localhost:9000/"
                                            ""
                                , body = raw
                                }
                                |> BackendTask.onError
                                    (\_ ->
                                        BackendTask.fail err
                                    )
                            )
                        <|
                            \_ -> task

                    else
                        BackendTask.fail err

                _ ->
                    BackendTask.fail err
        )
        task


stationIdsFromAutocomplete : BackendTask { fatal : FatalError, recoverable : Http.Error } (Dict String String)
stationIdsFromAutocomplete =
    List.range (Char.toCode 'A') (Char.toCode 'Z')
        |> List.map
            (\code ->
                wrapCache
                    (Viaggiatreno.Api.autocompletaStazioneInput
                        { params =
                            { input =
                                String.fromChar (Char.fromCode code)
                            }
                        }
                        |> BackendTask.quiet
                    )
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
