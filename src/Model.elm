module Model exposing
    ( Model
    , Msg(..)
    , PredictionsForStop(..)
    , Stop
    , encodeStop
    , streamEventDecoder
    )

import Browser
import Browser.Navigation as Navigation
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode
import Url exposing (Url)


type alias Model =
    { url : Url
    , navigationKey : Navigation.Key
    , stops : List ( Stop, PredictionsForStop )
    , routeIdFormText : String
    , stopIdFormText : String
    }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | AddStop Stop
    | TypeRouteId String
    | TypeStopId String
    | StreamEvent (Result Decode.Error StreamEvent)


type PredictionsForStop
    = Loading
    | Success (List Prediction)


type StreamEvent
    = Reset (List Prediction)
    | Add Prediction
    | Update Prediction
    | Remove String


type alias Prediction =
    { id : String
    , arrival_time : String
    , departure_time : String
    , stop : Stop
    }


type alias Stop =
    { routeId : String
    , stopId : String
    }


encodeStop : Stop -> Json.Encode.Value
encodeStop stop =
    Json.Encode.object
        [ ( "route_id", Json.Encode.string stop.routeId )
        , ( "stop_id", Json.Encode.string stop.stopId )
        ]


stopDecoder : Decode.Decoder Stop
stopDecoder =
    Decode.succeed Stop
        |> Pipeline.required "route_id" Decode.string
        |> Pipeline.required "stop_id" Decode.string


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
            Decode.map Add predictionDecoder

        "update" ->
            Decode.map Update predictionDecoder

        "remove" ->
            Decode.map Remove Decode.string

        _ ->
            Decode.fail ("unrecognized event name " ++ eventName)


predictionDecoder : Decode.Decoder Prediction
predictionDecoder =
    Decode.succeed Prediction
        |> Pipeline.required "id" Decode.string
        |> Pipeline.requiredAt [ "attributes", "arrival_time" ] Decode.string
        |> Pipeline.requiredAt [ "attributes", "departure_time" ] Decode.string
        |> Pipeline.custom
            (Decode.succeed Stop
                |> Pipeline.requiredAt [ "relationships", "route", "data", "id" ] Decode.string
                |> Pipeline.requiredAt [ "relationships", "stop", "data", "id" ] Decode.string
            )
