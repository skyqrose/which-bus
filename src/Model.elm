module Model exposing
    ( Model
    , Msg(..)
    , Prediction
    , PredictionsForStop
    , RouteId(..)
    , Stop
    , StopId(..)
    , PredictionsData(..)
    , PredictionsByStop
    , StreamEvent(..)
    , encodeStop
    , streamEventDecoder
    )

import AssocList as Dict
import Browser
import Browser.Navigation as Navigation
import Iso8601
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode
import Time
import Url exposing (Url)


type alias Model =
    { currentTime : Time.Posix
    , url : Url
    , navigationKey : Navigation.Key
    , stops : List Stop
    , predictionsData : PredictionsData
    , routeIdFormText : String
    , stopIdFormText : String
    }


type Msg
    = Tick Time.Posix
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | AddStop Stop
    | TypeRouteId String
    | TypeStopId String
    | StreamEvent (Result Decode.Error StreamEvent)


type PredictionsData
    = Loading
    | Failure Decode.Error
    | Success PredictionsByStop

type alias PredictionsByStop =
    Dict.Dict Stop PredictionsForStop


type alias PredictionsForStop =
    Dict.Dict PredictionId Prediction


type StreamEvent
    = Reset (List Prediction)
    | Insert Prediction
    | Remove PredictionId


type alias Prediction =
    { id : PredictionId
    , time : Time.Posix
    , stop : Stop
    }


type alias Stop =
    { routeId : RouteId
    , stopId : StopId
    }


type RouteId
    = RouteId String


type StopId
    = StopId String


type PredictionId
    = PredictionId String


encodeStop : Stop -> Json.Encode.Value
encodeStop stop =
    let
        (RouteId routeId) =
            stop.routeId

        (StopId stopId) =
            stop.stopId
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
            (Decode.succeed Stop
                |> Pipeline.requiredAt [ "relationships", "route", "data", "id" ] (Decode.map RouteId Decode.string)
                |> Pipeline.requiredAt [ "relationships", "stop", "data", "id" ] (Decode.map StopId Decode.string)
            )
