port module Api exposing
    ( ApiData
    , ApiResult(..)
    , Error(..)
    , Msg
    , init
    , makeUrl
    , predictionsForSelection
    , subscriptions
    , update
    )

import AssocList as Dict
import Data exposing (..)
import Iso8601
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode
import Time


port startStreamPort : String -> Cmd msg


port streamEventPort : (Decode.Value -> msg) -> Sub msg


type ApiResult
    = Loading
    | Failure Error
    | Success ApiData


type Error
    = DecodeError Decode.Error
    | BadOrder String


type alias ApiData =
    Dict.Dict PredictionId Prediction


type alias Msg =
    Result Decode.Error StreamEvent


type StreamEvent
    = Reset (List Prediction)
    | Insert Prediction
    | Remove PredictionId


type PredictionId
    = PredictionId String


type alias Prediction =
    { id : PredictionId
    , time : Time.Posix
    , selection : Selection
    }


makeUrl : String -> List ( String, String ) -> String
makeUrl path params =
    let
        base =
            "https://api-v3.mbta.com/"

        apiKey =
            "3a6d67c08111426d8617a30340a9fad3"

        paramsWithKey =
            ( "api_key", apiKey ) :: params
    in
    String.concat
        [ base
        , path
        , "?"
        , paramsWithKey
            |> List.map (\( param, value ) -> param ++ "=" ++ value)
            |> String.join "&"
        ]


init : List Selection -> ( ApiResult, Cmd msg )
init selections =
    ( Loading
    , startStream selections
    )


startStream : List Selection -> Cmd msg
startStream selections =
    let
        routeIds =
            selections
                |> List.map .routeId
                |> List.map (\(RouteId routeId) -> routeId)
                |> String.join ","

        stopIds =
            selections
                |> List.map .stopId
                |> List.map (\(StopId stopId) -> stopId)
                |> String.join ","

        url =
            makeUrl
                "predictions"
                [ ( "filter[route]", routeIds )
                , ( "filter[stop]", stopIds )
                ]
    in
    startStreamPort url


subscriptions : (Msg -> msg) -> Sub msg
subscriptions msg =
    streamEventPort (Decode.decodeValue streamEventDecoder >> msg)


update : Msg -> ApiResult -> ApiResult
update eventDecodeResult apiResult =
    case ( eventDecodeResult, apiResult ) of
        ( _, Failure error ) ->
            Failure error

        ( Err decodeError, _ ) ->
            Failure (DecodeError decodeError)

        ( Ok (Reset newPredictions), _ ) ->
            Success <|
                List.foldl insertPrediction Dict.empty newPredictions

        ( Ok (Insert newPrediction), Loading ) ->
            Failure (BadOrder "Insert while Loading")

        ( Ok (Remove predictionId), Loading ) ->
            Failure (BadOrder "Remove while Loading")

        ( Ok (Insert newPrediction), Success apiData ) ->
            Success <|
                insertPrediction newPrediction apiData

        ( Ok (Remove predictionId), Success apiData ) ->
            if Dict.member predictionId apiData then
                Success <|
                    Dict.remove predictionId apiData

            else
                Failure (BadOrder "Remove unknown id")


insertPrediction : Prediction -> ApiData -> ApiData
insertPrediction prediction predictionsDict =
    Dict.insert prediction.id prediction predictionsDict


predictionsForSelection : Selection -> ApiData -> List ShownPrediction
predictionsForSelection selection apiData =
    apiData
        |> Dict.values
        |> List.filter (\prediction -> prediction.selection == selection)
        |> List.map
            (\prediction ->
                { time = prediction.time
                }
            )



-- Decoding / Encoding


encodeSelection : Selection -> Json.Encode.Value
encodeSelection selection =
    let
        (RouteId routeId) =
            selection.routeId

        (StopId stopId) =
            selection.stopId
    in
    Json.Encode.object
        [ ( "route_id", Json.Encode.string routeId )
        , ( "stop_id", Json.Encode.string stopId )
        ]


streamEventDecoder : Decode.Decoder StreamEvent
streamEventDecoder =
    Decode.field "event" Decode.string
        |> Decode.andThen
            (\eventName ->
                Decode.field "data" (eventDataDecoder eventName)
            )


eventDataDecoder : String -> Decode.Decoder StreamEvent
eventDataDecoder eventName =
    case eventName of
        "reset" ->
            Decode.map Reset (Decode.list predictionDecoder)

        "add" ->
            Decode.map Insert predictionDecoder

        "update" ->
            Decode.map Insert predictionDecoder

        "remove" ->
            Decode.map Remove (Decode.map PredictionId Decode.string)

        _ ->
            Decode.fail ("unrecognized event name " ++ eventName)


predictionDecoder : Decode.Decoder Prediction
predictionDecoder =
    Decode.succeed Prediction
        |> Pipeline.required "id" (Decode.map PredictionId Decode.string)
        |> Pipeline.custom
            (Decode.oneOf
                [ Decode.at [ "attributes", "arrival_time" ] Iso8601.decoder
                , Decode.at [ "attributes", "departure_time" ] Iso8601.decoder
                ]
            )
        |> Pipeline.custom
            (Decode.succeed Selection
                |> Pipeline.requiredAt [ "relationships", "route", "data", "id" ] (Decode.map RouteId Decode.string)
                |> Pipeline.requiredAt [ "relationships", "stop", "data", "id" ] (Decode.map StopId Decode.string)
            )
