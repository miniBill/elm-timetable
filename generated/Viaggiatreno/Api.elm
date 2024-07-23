module Viaggiatreno.Api exposing (autocompletaStazioneInput, autocompletaStazioneNTSInput, cercaStazioneInput, dettaglioStazioneIdStazioneIdRegione, elencoStazioniIdRegione, regioneIdStazione)

{-| 
## stations


@docs autocompletaStazioneInput, autocompletaStazioneNTSInput, cercaStazioneInput
@docs dettaglioStazioneIdStazioneIdRegione, elencoStazioniIdRegione, regioneIdStazione
-}


import BackendTask
import BackendTask.Http
import FatalError
import Json.Decode
import Url.Builder
import Viaggiatreno.Json
import Viaggiatreno.Types


{-| Get possible autocompletions

Get possible autocompletions for a given query
-}
autocompletaStazioneInput :
    { params : { input : String } }
    -> BackendTask.BackendTask { fatal : FatalError.FatalError
    , recoverable : BackendTask.Http.Error
    } String
autocompletaStazioneInput config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin
                "http://www.viaggiatreno.it/infomobilita/resteasy/viaggiatreno"
                [ "autocompletaStazione", config.params.input ]
                []
        , method = "GET"
        , headers = []
        , body = BackendTask.Http.emptyBody
        , retries = Nothing
        , timeoutInMs = Nothing
        }
        BackendTask.Http.expectString


{-| Get possible autocompletions

Get possible autocompletions for a given query
-}
autocompletaStazioneNTSInput :
    { params : { input : String } }
    -> BackendTask.BackendTask { fatal : FatalError.FatalError
    , recoverable : BackendTask.Http.Error
    } String
autocompletaStazioneNTSInput config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin
                "http://www.viaggiatreno.it/infomobilita/resteasy/viaggiatreno"
                [ "autocompletaStazioneNTS", config.params.input ]
                []
        , method = "GET"
        , headers = []
        , body = BackendTask.Http.emptyBody
        , retries = Nothing
        , timeoutInMs = Nothing
        }
        BackendTask.Http.expectString


{-| Get possible stations

Get possible stations for a given query
-}
cercaStazioneInput :
    { params : { input : String } }
    -> BackendTask.BackendTask { fatal : FatalError.FatalError
    , recoverable : BackendTask.Http.Error
    } (List Viaggiatreno.Types.Localita)
cercaStazioneInput config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin
                "http://www.viaggiatreno.it/infomobilita/resteasy/viaggiatreno"
                [ "cercaStazione", config.params.input ]
                []
        , method = "GET"
        , headers = []
        , body = BackendTask.Http.emptyBody
        , retries = Nothing
        , timeoutInMs = Nothing
        }
        (BackendTask.Http.expectJson
             (Json.Decode.list Viaggiatreno.Json.decodeLocalita)
        )


{-| Get station details -}
dettaglioStazioneIdStazioneIdRegione :
    { params : { idStazione : String, idRegione : Int } }
    -> BackendTask.BackendTask { fatal : FatalError.FatalError
    , recoverable : BackendTask.Http.Error
    } Viaggiatreno.Types.StationDetails
dettaglioStazioneIdStazioneIdRegione config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin
                "http://www.viaggiatreno.it/infomobilita/resteasy/viaggiatreno"
                [ "dettaglioStazione"
                , config.params.idStazione
                , String.fromInt config.params.idRegione
                ]
                []
        , method = "GET"
        , headers = []
        , body = BackendTask.Http.emptyBody
        , retries = Nothing
        , timeoutInMs = Nothing
        }
        (BackendTask.Http.expectJson Viaggiatreno.Json.decodeStationDetails)


{-| Get all stations in a region -}
elencoStazioniIdRegione :
    { params : { idRegione : Int } }
    -> BackendTask.BackendTask { fatal : FatalError.FatalError
    , recoverable : BackendTask.Http.Error
    } (List Viaggiatreno.Types.StationDetails)
elencoStazioniIdRegione config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin
                "http://www.viaggiatreno.it/infomobilita/resteasy/viaggiatreno"
                [ "elencoStazioni", String.fromInt config.params.idRegione ]
                []
        , method = "GET"
        , headers = []
        , body = BackendTask.Http.emptyBody
        , retries = Nothing
        , timeoutInMs = Nothing
        }
        (BackendTask.Http.expectJson
             (Json.Decode.list Viaggiatreno.Json.decodeStationDetails)
        )


{-| Get the idRegione for an idStazione -}
regioneIdStazione :
    { params : { idStazione : String } }
    -> BackendTask.BackendTask { fatal : FatalError.FatalError
    , recoverable : BackendTask.Http.Error
    } Int
regioneIdStazione config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin
                "http://www.viaggiatreno.it/infomobilita/resteasy/viaggiatreno"
                [ "regione", config.params.idStazione ]
                []
        , method = "GET"
        , headers = []
        , body = BackendTask.Http.emptyBody
        , retries = Nothing
        , timeoutInMs = Nothing
        }
        (BackendTask.Http.expectJson Json.Decode.int)