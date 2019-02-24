module Model exposing
    ( Model
    , Msg(..)
    , Prediction
    , PredictionsBySelection
    , PredictionsData(..)
    , PredictionsForSelection
    , RouteId(..)
    , Selection
    , StopId(..)
    , StreamEvent(..)
    , encodeSelection
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
    , selections : List Selection
    , predictionsData : PredictionsData
    , routeIdFormText : String
    , stopIdFormText : String
    }


type Msg
    = Tick Time.Posix
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | AddSelection Selection
    | TypeRouteId String
    | TypeStopId String
    | StreamEvent (Result Decode.Error StreamEvent)


type PredictionsData
    = Loading
    | Failure Decode.Error
    | Success PredictionsBySelection


type alias PredictionsBySelection =
    Dict.Dict Selection PredictionsForSelection


type alias PredictionsForSelection =
    Dict.Dict PredictionId Prediction


type StreamEvent
    = Reset (List Prediction)
    | Insert Prediction
    | Remove PredictionId


type alias Prediction =
    { id : PredictionId
    , time : Time.Posix
    , selection : Selection
    }


type alias Selection =
    { routeId : RouteId
    , stopId : StopId
    }


type RouteId
    = RouteId String


type StopId
    = StopId String


type PredictionId
    = PredictionId String


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
