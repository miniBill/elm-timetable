module ViaggiaTreno exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Http as Http
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import List.Extra
import Pages.Script as Script exposing (Script)
import Viaggiatreno.Api
import Viaggiatreno.Types exposing (StationDetails)


run : Script
run =
    Script.withoutCliOptions script


script : BackendTask FatalError ()
script =
    Do.log "Getting stations from autocomplete" <| \_ ->
    Do.allowFatal stationIdsFromAutocomplete <| \fromAutocomplete ->
    Do.log ("Got " ++ String.fromInt (Dict.size fromAutocomplete) ++ " train stations from autocomplete") <| \_ ->
    Do.allowFatal (getAllStationsDetails (Dict.keys fromAutocomplete)) <| \details ->
    Script.log
        ("Missing from regions:"
            ++ Debug.toString
                (List.take 3 details)
        )


getAllStationsDetails :
    List String
    -> BackendTask { fatal : FatalError, recoverable : Http.Error } (List StationDetails)
getAllStationsDetails stationIds =
    stationIds
        |> List.map
            (\stationId ->
                Do.do
                    (wrapCache
                        (Viaggiatreno.Api.regioneStationId
                            { params = { stationId = stationId } }
                         -- |> BackendTask.quiet
                        )
                    )
                <| \regionId ->
                wrapCache
                    (Viaggiatreno.Api.dettaglioStazioneStationIdRegionId
                        { params =
                            { stationId = stationId
                            , regionId = regionId
                            }
                        }
                    )
             -- |> BackendTask.quiet
            )
        |> combineInGroupsOf 1


combineInGroupsOf :
    Int
    -> List (BackendTask e v)
    -> BackendTask e (List v)
combineInGroupsOf size list =
    list
        |> List.Extra.greedyGroupsOf size
        |> List.map BackendTask.combine
        |> BackendTask.sequence
        |> BackendTask.map List.concat


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
