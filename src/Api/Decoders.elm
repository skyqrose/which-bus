module Api.Decoders exposing
    ( directionIdDecoder
    , predictionDecoder
    , predictionIdDecoder
    , resourceDecoder
    , resourceIdDecoder
    , routeIdDecoder
    , stopDecoder
    , stopIdDecoder
    , tripDecoder
    , tripIdDecoder
    )

import Api.Types exposing (..)
import Iso8601
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


resourceIdDecoder : Decode.Decoder ResourceId
resourceIdDecoder =
    Decode.map2
        Tuple.pair
        (Decode.at [ "type" ] Decode.string)
        (Decode.at [ "id" ] Decode.string)
        |> Decode.andThen
            (\( typeString, id ) ->
                case typeString of
                    "prediction" ->
                        Decode.succeed (ResourcePredictionId (PredictionId id))

                    "trip" ->
                        Decode.succeed (ResourceTripId (TripId id))

                    "stop" ->
                        Decode.succeed (ResourceStopId (StopId id))

                    otherType ->
                        Decode.fail ("unrecognized type " ++ otherType)
            )


resourceDecoder : Decode.Decoder Resource
resourceDecoder =
    Decode.at [ "type" ] Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "prediction" ->
                        Decode.map ResourcePrediction predictionDecoder

                    "trip" ->
                        Decode.map ResourceTrip tripDecoder

                    "stop" ->
                        Decode.map ResourceStop stopDecoder

                    otherType ->
                        Decode.fail ("unrecognized type " ++ otherType)
            )


predictionIdDecoder : Decode.Decoder PredictionId
predictionIdDecoder =
    Decode.map PredictionId Decode.string


tripIdDecoder : Decode.Decoder TripId
tripIdDecoder =
    Decode.map TripId Decode.string


stopIdDecoder : Decode.Decoder StopId
stopIdDecoder =
    Decode.map StopId Decode.string


routeIdDecoder : Decode.Decoder RouteId
routeIdDecoder =
    Decode.map RouteId Decode.string


directionIdDecoder : Decode.Decoder DirectionId
directionIdDecoder =
    Decode.int
        |> Decode.andThen
            (\x ->
                case x of
                    0 ->
                        Decode.succeed D0

                    1 ->
                        Decode.succeed D1

                    _ ->
                        Decode.fail ("unrecognized direction_id " ++ String.fromInt x)
            )


predictionDecoder : Decode.Decoder Prediction
predictionDecoder =
    checkType "prediction" Prediction
        |> Pipeline.required "id" predictionIdDecoder
        |> Pipeline.custom
            (Decode.oneOf
                [ Decode.at [ "attributes", "arrival_time" ] Iso8601.decoder
                , Decode.at [ "attributes", "departure_time" ] Iso8601.decoder
                ]
            )
        |> Pipeline.requiredAt [ "relationships", "route", "data", "id" ] routeIdDecoder
        |> Pipeline.requiredAt [ "relationships", "stop", "data", "id" ] stopIdDecoder
        |> Pipeline.requiredAt [ "attributes", "direction_id" ] directionIdDecoder
        |> Pipeline.requiredAt [ "relationships", "trip", "data", "id" ] tripIdDecoder


tripDecoder : Decode.Decoder Trip
tripDecoder =
    checkType "trip" Trip
        |> Pipeline.required "id" tripIdDecoder
        |> Pipeline.requiredAt [ "attributes", "headsign" ] Decode.string


stopDecoder : Decode.Decoder Stop
stopDecoder =
    checkType "stop" Stop
        |> Pipeline.required "id" stopIdDecoder
        |> Pipeline.requiredAt [ "attributes", "name" ] Decode.string
        |> Pipeline.optionalAt [ "relationships", "parent_station", "data", "id" ] (Decode.map Just stopIdDecoder) Nothing


{-| Fails decoding if the json api type is not as expected.
Replaces `Decode.succeed` in a pipeline
-}
checkType : String -> a -> Decode.Decoder a
checkType expectedTypeString resourceConstructor =
    Decode.at [ "type" ] Decode.string
        |> Decode.andThen
            (\actualTypeString ->
                if expectedTypeString == actualTypeString then
                    Decode.succeed resourceConstructor

                else
                    Decode.fail ("expected type " ++ expectedTypeString ++ " but got " ++ actualTypeString)
            )
