module Viaggiatreno.Types exposing
    ( Localita, News, StationDetails
    , AutocompletaStazioneInput_Error, AutocompletaStazioneNTSInput_Error, CercaStazioneInput_Error
    , DettaglioStazioneStationIdRegionId_Error, ElencoStazioniRegionId_Error, NewsRegionIdLanguage_Error
    , RegioneStationId_Error
    )

{-|


## Aliases

@docs Localita, News, StationDetails


## Errors

@docs AutocompletaStazioneInput_Error, AutocompletaStazioneNTSInput_Error, CercaStazioneInput_Error
@docs DettaglioStazioneStationIdRegionId_Error, ElencoStazioniRegionId_Error, NewsRegionIdLanguage_Error
@docs RegioneStationId_Error

-}

import OpenApi.Common


type alias AutocompletaStazioneInput_Error =
    Never


type alias AutocompletaStazioneNTSInput_Error =
    Never


type alias CercaStazioneInput_Error =
    Never


type alias DettaglioStazioneStationIdRegionId_Error =
    Never


type alias ElencoStazioniRegionId_Error =
    Never


type alias NewsRegionIdLanguage_Error =
    Never


type alias RegioneStationId_Error =
    Never


type alias StationDetails =
    { codReg : Int
    , codStazione : String
    , codiceStazione : String
    , esterno : Bool
    , lat : Float
    , localita : Localita
    , lon : Float
    , nomeCitta : String
    , tipoStazione : Int
    }


type alias News =
    { data : Int, primoPiano : String, testo : String, titolo : String }


type alias Localita =
    { id : String
    , label : OpenApi.Common.Nullable String
    , nomeBreve : String
    , nomeLungo : String
    }
