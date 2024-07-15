module TransitLand exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Http as Http
import Cli.Option
import Cli.OptionsParser
import Cli.Program exposing (Config)
import FatalError exposing (FatalError)
import Json.Decode
import Json.Decode.Pipeline
import Pages.Script as Script exposing (Script)
import Url.Builder


run : Script
run =
    Script.withCliOptions config task


type alias CliOptions =
    { apikey : String }


config : Config CliOptions
config =
    Cli.Program.config
        |> Cli.Program.add
            (Cli.OptionsParser.build CliOptions
                |> Cli.OptionsParser.with (Cli.Option.requiredKeywordArg "apikey")
            )


task : CliOptions -> BackendTask FatalError ()
task cliOptions =
    paginated cliOptions
        { headers = []
        , path = [ "feeds.gtfs" ]
        , key = "feeds"
        , decoder = feedInfoDecoder
        }
        |> BackendTask.allowFatal
        |> BackendTask.andThen (Debug.toString >> Script.log)


paginated :
    CliOptions
    ->
        { headers : List ( String, String )
        , path : List String
        , key : String
        , decoder : Json.Decode.Decoder value
        }
    -> BackendTask { fatal : FatalError, recoverable : Http.Error } (List value)
paginated { apikey } { headers, path, key, decoder } =
    let
        go url =
            Http.getWithOptions
                { url = url
                , headers = ( "apikey", apikey ) :: headers
                , expect =
                    Http.expectJson
                        (Json.Decode.map2 (\list next -> { list = list, next = next })
                            (Json.Decode.field key (Json.Decode.list decoder))
                            (Json.Decode.maybe (Json.Decode.at [ "meta", "next" ] Json.Decode.string))
                        )
                , timeoutInMs = Nothing
                , retries = Nothing
                , cacheStrategy = Just Http.ForceCache
                , cachePath = Just ".cache"
                }
                |> BackendTask.andThen
                    (\{ list, next } ->
                        case next of
                            Nothing ->
                                BackendTask.succeed list

                            Just nextUrl ->
                                go nextUrl
                                    |> BackendTask.map (\l -> list ++ l)
                    )
    in
    go (Url.Builder.crossOrigin "https://transit.land/api/v2/rest" path [ Url.Builder.int "limit" 100 ])


type alias FeedInfo =
    { id : Int
    , name : Maybe String
    , spec : String
    , urls : Urls
    , license : Maybe License
    , feedState : Maybe FeedState
    }


feedInfoDecoder : Json.Decode.Decoder FeedInfo
feedInfoDecoder =
    Json.Decode.succeed FeedInfo
        |> Json.Decode.Pipeline.required "id" Json.Decode.int
        |> Json.Decode.Pipeline.optional "name" (Json.Decode.map Just Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.required "spec" Json.Decode.string
        |> Json.Decode.Pipeline.required "urls" urlsDecoder
        |> Json.Decode.Pipeline.required "license" licenseDecoder
        |> Json.Decode.Pipeline.required "feed_state" feedStateDecoder


type alias Urls =
    { staticCurrent : String
    , staticPlanned : Maybe String
    }


urlsDecoder : Json.Decode.Decoder Urls
urlsDecoder =
    Json.Decode.succeed Urls
        |> Json.Decode.Pipeline.required "static_current" Json.Decode.string
        |> Json.Decode.Pipeline.optional "static_planned" (Json.Decode.map Just Json.Decode.string) Nothing


type alias License =
    { spdxIdentifier : String
    , url : Maybe String
    }


licenseDecoder : Json.Decode.Decoder (Maybe License)
licenseDecoder =
    Json.Decode.succeed License
        |> Json.Decode.Pipeline.required "spdx_identifier" Json.Decode.string
        |> Json.Decode.Pipeline.optional "url" (Json.Decode.map Just Json.Decode.string) Nothing
        |> Json.Decode.map
            (\license ->
                if license == emptyLicense then
                    Nothing

                else
                    Just license
            )


emptyLicense : License
emptyLicense =
    { spdxIdentifier = ""
    , url = Nothing
    }


type alias FeedState =
    { lastFetchError : Maybe String
    , lastFetchedAt : Maybe String
    , lastSuccessfulFetchAt : Maybe String
    }


feedStateDecoder : Json.Decode.Decoder (Maybe FeedState)
feedStateDecoder =
    Json.Decode.succeed FeedState
        |> Json.Decode.Pipeline.optional "last_fetch_error" (Json.Decode.map Just Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "last_fetched_at" (Json.Decode.map Just Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "last_successful_fetch_at" (Json.Decode.map Just Json.Decode.string) Nothing
        |> Json.Decode.map
            (\feedState ->
                if feedState == emptyFeedState then
                    Nothing

                else
                    Just feedState
            )


emptyFeedState : FeedState
emptyFeedState =
    { lastFetchedAt = Nothing
    , lastFetchError = Nothing
    , lastSuccessfulFetchAt = Nothing
    }
