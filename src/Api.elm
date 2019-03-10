port module Api exposing
    ( ApiData
    , ApiResult(..)
    , Msg
    , PredictionsBySelection
    , PredictionsForSelection
    , init
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
    PredictionsBySelection


type alias Msg =
    Result Decode.Error StreamEvent


type alias PredictionsBySelection =
    Dict.Dict Selection PredictionsForSelection


type alias PredictionsForSelection =
    Dict.Dict PredictionId Prediction


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
update decodeResult apiResult =
    case decodeResult of
        Ok event ->
            let
                _ =
                    Debug.log "successfully decoded" event
            in
            applyStreamEvent event apiResult

        Err error ->
            let
                _ =
                    Debug.log "failed to decode" (Debug.toString error)
            in
            Failure error


applyStreamEvent : StreamEvent -> ApiResult -> ApiResult
applyStreamEvent event apiResult =
    case ( event, apiResult ) of
        ( _, Failure error ) ->
            Failure error

        ( Reset newPredictions, _ ) ->
            Success <|
                List.foldl insertPrediction Dict.empty newPredictions

        ( Insert newPrediction, Loading ) ->
            Loading

        ( Insert newPrediction, Success predictionsBySelection ) ->
            Success <|
                insertPrediction newPrediction predictionsBySelection

        ( Remove predictionId, Loading ) ->
            Loading

        ( Remove predictionId, Success predictionsBySelection ) ->
            -- We don't know which selection this prediction was for
            -- So we have to search all the selections for it.
            Success <|
                Dict.map
                    (\selection predictionsForSelection ->
                        Dict.remove predictionId predictionsForSelection
                    )
                    predictionsBySelection


insertPrediction : Prediction -> PredictionsBySelection -> PredictionsBySelection
insertPrediction prediction predictionsBySelection =
    Dict.update
        prediction.selection
        (\maybePredictionsForSelection ->
            case maybePredictionsForSelection of
                Nothing ->
                    Just (Dict.singleton prediction.id prediction)

                Just predictionsForSelection ->
                    Just (Dict.insert prediction.id prediction predictionsForSelection)
        )
        predictionsBySelection



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
