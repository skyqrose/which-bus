port module Api exposing
    ( ApiData
    , ApiResult(..)
    , Msg
    , init
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


port startStreamPort : String -> Cmd msg


port streamEventPort : (Decode.Value -> msg) -> Sub msg


type ApiResult
    = Loading
    | Failure Decode.Error
    | Success ApiData


type alias ApiData =
    Dict.Dict PredictionId Prediction


type alias Msg =
    Result Decode.Error StreamEvent


type StreamEvent
    = Reset (List Prediction)
    | Insert Prediction
    | Remove PredictionId


init : List Selection -> ( ApiResult, Cmd msg )
init selections =
    ( Loading
    , startStream selections
    )


startStream : List Selection -> Cmd msg
startStream selections =
    let
        api_key =
            "3a6d67c08111426d8617a30340a9fad3"

        route_ids =
            selections
                |> List.map .routeId
                |> List.map (\(RouteId routeId) -> routeId)
                |> String.join ","

        stop_ids =
            selections
                |> List.map .stopId
                |> List.map (\(StopId stopId) -> stopId)
                |> String.join ","

        url =
            "https://api-v3.mbta.com/predictions"
                ++ "?api_key="
                ++ api_key
                ++ "&filter[route]="
                ++ route_ids
                ++ "&filter[stop]="
                ++ stop_ids
    in
    startStreamPort url


subscriptions : (Msg -> msg) -> Sub msg
subscriptions msg =
    streamEventPort (Decode.decodeValue streamEventDecoder >> msg)


update : Msg -> ApiResult -> ApiResult
update eventDecodeResult apiResult =
    let
        _ =
            case eventDecodeResult of
                Ok event ->
                    Debug.log "successfully decoded" (Ok event)

                Err error ->
                    Debug.log "failed to decode" (Err error)
    in
    case ( eventDecodeResult, apiResult ) of
        ( _, Failure error ) ->
            Failure error

        ( Err decodeError, _ ) ->
            Failure decodeError

        ( Ok (Reset newPredictions), _ ) ->
            Success <|
                List.foldl insertPrediction Dict.empty newPredictions

        ( Ok (Insert newPrediction), Loading ) ->
            Loading

        ( Ok (Insert newPrediction), Success apiData ) ->
            Success <|
                insertPrediction newPrediction apiData

        ( Ok (Remove predictionId), Loading ) ->
            Loading

        ( Ok (Remove predictionId), Success apiData ) ->
            Success <|
                Dict.remove predictionId apiData


insertPrediction : Prediction -> ApiData -> ApiData
insertPrediction prediction predictionsDict =
    Dict.insert prediction.id prediction predictionsDict


predictionsForSelection : Selection -> ApiData -> List Prediction
predictionsForSelection selection apiData =
    apiData
        |> Dict.values
        |> List.filter (\prediction -> prediction.selection == selection)



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
