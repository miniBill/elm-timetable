module MobilityDatabase exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import BackendTask.Http as Http
import Csv.Decode
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import SHA256


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    getCached "https://bit.ly/catalogs-csv"
        |> BackendTask.andThen
            (\raw ->
                case Csv.Decode.decodeCsv Csv.Decode.FieldNamesFromFirstRow feedsDecoder raw of
                    Err e ->
                        BackendTask.fail (FatalError.fromString <| Csv.Decode.errorToString e)

                    Ok lines ->
                        lines
                            |> List.filter
                                (\{ dataType, country, status } ->
                                    (dataType == "gtfs")
                                        && List.member country [ "IT", "AT", "DE", "NL" ]
                                        && (case status of
                                                "inactive" ->
                                                    False

                                                "deprecated" ->
                                                    False

                                                "" ->
                                                    True

                                                _ ->
                                                    Debug.todo <| "Unknown status: " ++ status
                                           )
                                )
                            |> List.sortBy .country
                            |> List.map (\{ country, url, provider } -> "[" ++ country ++ "] " ++ provider ++ " " ++ url)
                            |> List.map Script.log
                            |> BackendTask.doEach
            )


type alias FeedInfo =
    { dataType : String
    , country : String
    , url : String
    , status : String
    , provider : String
    }


feedsDecoder : Csv.Decode.Decoder FeedInfo
feedsDecoder =
    Csv.Decode.succeed FeedInfo
        |> Csv.Decode.pipeline (Csv.Decode.field "data_type" Csv.Decode.string)
        |> Csv.Decode.pipeline (Csv.Decode.field "location.country_code" Csv.Decode.string)
        |> Csv.Decode.pipeline (Csv.Decode.field "urls.direct_download" Csv.Decode.string)
        |> Csv.Decode.pipeline (Csv.Decode.field "status" Csv.Decode.string)
        |> Csv.Decode.pipeline (Csv.Decode.field "provider" Csv.Decode.string)


getCached :
    String
    -> BackendTask FatalError String
getCached url =
    let
        path : String
        path =
            ".cache/" ++ (SHA256.fromString url |> SHA256.toHex) ++ ".csv"
    in
    File.rawFile path
        |> BackendTask.onError
            (\err ->
                case err.recoverable of
                    File.FileDoesntExist ->
                        Http.getWithOptions
                            { url = url
                            , expect = Http.expectString
                            , headers = []
                            , timeoutInMs = Nothing
                            , retries = Nothing
                            , cacheStrategy = Nothing
                            , cachePath = Nothing
                            }
                            |> BackendTask.allowFatal
                            |> BackendTask.andThen
                                (\res ->
                                    Script.writeFile { path = path, body = res }
                                        |> BackendTask.allowFatal
                                        |> BackendTask.map (\_ -> res)
                                )

                    _ ->
                        BackendTask.fail err.fatal
            )
