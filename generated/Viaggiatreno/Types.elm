module Viaggiatreno.Types exposing
    ( Localita, StationDetails
    , AutocompletaStazioneInput_Error, AutocompletaStazioneNTSInput_Error, CercaStazioneInput_Error
    , DettaglioStazioneIdStazioneIdRegione_Error, ElencoStazioniIdRegione_Error, RegioneIdStazione_Error
    )

{-|


## Aliases

@docs Localita, StationDetails


## Errors

@docs AutocompletaStazioneInput_Error, AutocompletaStazioneNTSInput_Error, CercaStazioneInput_Error
@docs DettaglioStazioneIdStazioneIdRegione_Error, ElencoStazioniIdRegione_Error, RegioneIdStazione_Error

-}

import OpenApi.Common


type alias AutocompletaStazioneInput_Error =
    Never


type alias AutocompletaStazioneNTSInput_Error =
    Never


type alias CercaStazioneInput_Error =
    Never


type alias DettaglioStazioneIdStazioneIdRegione_Error =
    Never


type alias ElencoStazioniIdRegione_Error =
    Never


type alias RegioneIdStazione_Error =
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


type alias Localita =
    { id : String
    , label : OpenApi.Common.Nullable String
    , nomeBreve : String
    , nomeLungo : String
    }
