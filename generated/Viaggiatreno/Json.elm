module Viaggiatreno.Json exposing (decodeLocalita, decodeStationDetails, encodeLocalita, encodeStationDetails)

{-| 
## Encoders


@docs encodeLocalita, encodeStationDetails

## Decoders


@docs decodeLocalita, decodeStationDetails
-}


import Json.Decode
import Json.Encode
import OpenApi.Common
import Viaggiatreno.Types


decodeStationDetails : Json.Decode.Decoder Viaggiatreno.Types.StationDetails
decodeStationDetails =
    Json.Decode.succeed
        (\codReg codStazione codiceStazione esterno lat localita lon nomeCitta tipoStazione ->
             { codReg = codReg
             , codStazione = codStazione
             , codiceStazione = codiceStazione
             , esterno = esterno
             , lat = lat
             , localita = localita
             , lon = lon
             , nomeCitta = nomeCitta
             , tipoStazione = tipoStazione
             }
        ) |> OpenApi.Common.jsonDecodeAndMap
                     (Json.Decode.field "codReg" Json.Decode.int
                     ) |> OpenApi.Common.jsonDecodeAndMap
                                  (Json.Decode.field
                                           "codStazione"
                                           Json.Decode.string
                                  ) |> OpenApi.Common.jsonDecodeAndMap
                                               (Json.Decode.field
                                                        "codiceStazione"
                                                        Json.Decode.string
                                               ) |> OpenApi.Common.jsonDecodeAndMap
                                                            (Json.Decode.field
                                                                     "esterno"
                                                                     Json.Decode.bool
                                                            ) |> OpenApi.Common.jsonDecodeAndMap
                                                                         (Json.Decode.field
                                                                                  "lat"
                                                                                  Json.Decode.float
                                                                         ) |> OpenApi.Common.jsonDecodeAndMap
                                                                                      (Json.Decode.field
                                                                                               "localita"
                                                                                               decodeLocalita
                                                                                      ) |> OpenApi.Common.jsonDecodeAndMap
                                                                                                   (Json.Decode.field
                                                                                                            "lon"
                                                                                                            Json.Decode.float
                                                                                                   ) |> OpenApi.Common.jsonDecodeAndMap
                                                                                                                (Json.Decode.field
                                                                                                                         "nomeCitta"
                                                                                                                         Json.Decode.string
                                                                                                                ) |> OpenApi.Common.jsonDecodeAndMap
                                                                                                                             (Json.Decode.field
                                                                                                                                      "tipoStazione"
                                                                                                                                      Json.Decode.int
                                                                                                                             )


encodeStationDetails : Viaggiatreno.Types.StationDetails -> Json.Encode.Value
encodeStationDetails rec =
    Json.Encode.object
        [ ( "codReg", Json.Encode.int rec.codReg )
        , ( "codStazione", Json.Encode.string rec.codStazione )
        , ( "codiceStazione", Json.Encode.string rec.codiceStazione )
        , ( "esterno", Json.Encode.bool rec.esterno )
        , ( "lat", Json.Encode.float rec.lat )
        , ( "localita", encodeLocalita rec.localita )
        , ( "lon", Json.Encode.float rec.lon )
        , ( "nomeCitta", Json.Encode.string rec.nomeCitta )
        , ( "tipoStazione", Json.Encode.int rec.tipoStazione )
        ]


decodeLocalita : Json.Decode.Decoder Viaggiatreno.Types.Localita
decodeLocalita =
    Json.Decode.succeed
        (\id label nomeBreve nomeLungo ->
             { id = id
             , label = label
             , nomeBreve = nomeBreve
             , nomeLungo = nomeLungo
             }
        ) |> OpenApi.Common.jsonDecodeAndMap
                     (Json.Decode.field "id" Json.Decode.string
                     ) |> OpenApi.Common.jsonDecodeAndMap
                                  (Json.Decode.field
                                           "label"
                                           (Json.Decode.oneOf
                                                    [ Json.Decode.map
                                                        OpenApi.Common.Present
                                                        Json.Decode.string
                                                    , Json.Decode.null
                                                        OpenApi.Common.Null
                                                    ]
                                           )
                                  ) |> OpenApi.Common.jsonDecodeAndMap
                                               (Json.Decode.field
                                                        "nomeBreve"
                                                        Json.Decode.string
                                               ) |> OpenApi.Common.jsonDecodeAndMap
                                                            (Json.Decode.field
                                                                     "nomeLungo"
                                                                     Json.Decode.string
                                                            )


encodeLocalita : Viaggiatreno.Types.Localita -> Json.Encode.Value
encodeLocalita rec =
    Json.Encode.object
        [ ( "id", Json.Encode.string rec.id )
        , ( "label"
          , case rec.label of
                OpenApi.Common.Null ->
                    Json.Encode.null

                OpenApi.Common.Present value ->
                    Json.Encode.string value
          )
        , ( "nomeBreve", Json.Encode.string rec.nomeBreve )
        , ( "nomeLungo", Json.Encode.string rec.nomeLungo )
        ]