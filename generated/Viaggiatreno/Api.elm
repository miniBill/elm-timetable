module Viaggiatreno.Api exposing
    ( newsRegionIdLanguage
    , autocompletaStazioneInput, autocompletaStazioneNTSInput, cercaStazioneInput
    , dettaglioStazioneStationIdRegionId, elencoStazioniRegionId, regioneStationId
    )

{-|


## news

@docs newsRegionIdLanguage


## stations

@docs autocompletaStazioneInput, autocompletaStazioneNTSInput, cercaStazioneInput
@docs dettaglioStazioneStationIdRegionId, elencoStazioniRegionId, regioneStationId

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
    ->
        BackendTask.BackendTask
            { fatal : FatalError.FatalError
            , recoverable : BackendTask.Http.Error
            }
            String
autocompletaStazioneInput config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin
                "http://localhost:9000/cache"
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
    ->
        BackendTask.BackendTask
            { fatal : FatalError.FatalError
            , recoverable : BackendTask.Http.Error
            }
            String
autocompletaStazioneNTSInput config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin
                "http://localhost:9000/cache"
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
    ->
        BackendTask.BackendTask
            { fatal : FatalError.FatalError
            , recoverable : BackendTask.Http.Error
            }
            (List Viaggiatreno.Types.Localita)
cercaStazioneInput config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin
                "http://localhost:9000/cache"
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


{-| Get station details
-}
dettaglioStazioneStationIdRegionId :
    { params : { stationId : String, regionId : Int } }
    ->
        BackendTask.BackendTask
            { fatal : FatalError.FatalError
            , recoverable : BackendTask.Http.Error
            }
            Viaggiatreno.Types.StationDetails
dettaglioStazioneStationIdRegionId config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin
                "http://localhost:9000/cache"
                [ "dettaglioStazione"
                , config.params.stationId
                , String.fromInt config.params.regionId
                ]
                []
        , method = "GET"
        , headers = []
        , body = BackendTask.Http.emptyBody
        , retries = Nothing
        , timeoutInMs = Nothing
        }
        (BackendTask.Http.expectJson Viaggiatreno.Json.decodeStationDetails)


{-| |- Get all stations in a region [WARNING: incomplete listing]
-}
elencoStazioniRegionId :
    { params : { regionId : Int } }
    ->
        BackendTask.BackendTask
            { fatal : FatalError.FatalError
            , recoverable : BackendTask.Http.Error
            }
            (List Viaggiatreno.Types.StationDetails)
elencoStazioniRegionId config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin
                "http://localhost:9000/cache"
                [ "elencoStazioni", String.fromInt config.params.regionId ]
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


{-| Get news for a specific region and language (use 0 for national news)
-}
newsRegionIdLanguage :
    { params : { regionId : Int, language : String } }
    ->
        BackendTask.BackendTask
            { fatal : FatalError.FatalError
            , recoverable : BackendTask.Http.Error
            }
            (List Viaggiatreno.Types.News)
newsRegionIdLanguage config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin
                "http://localhost:9000/cache"
                [ "news"
                , String.fromInt config.params.regionId
                , config.params.language
                ]
                []
        , method = "GET"
        , headers = []
        , body = BackendTask.Http.emptyBody
        , retries = Nothing
        , timeoutInMs = Nothing
        }
        (BackendTask.Http.expectJson
            (Json.Decode.list Viaggiatreno.Json.decodeNews)
        )


{-| Get the regionId for an stationId
-}
regioneStationId :
    { params : { stationId : String } }
    ->
        BackendTask.BackendTask
            { fatal : FatalError.FatalError
            , recoverable : BackendTask.Http.Error
            }
            Int
regioneStationId config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin
                "http://localhost:9000/cache"
                [ "regione", config.params.stationId ]
                []
        , method = "GET"
        , headers = []
        , body = BackendTask.Http.emptyBody
        , retries = Nothing
        , timeoutInMs = Nothing
        }
        (BackendTask.Http.expectJson Json.Decode.int)
