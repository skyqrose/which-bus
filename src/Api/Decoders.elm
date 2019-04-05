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
    , vehicleDecoder
    , vehicleIdDecoder
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

                    "stop" ->
                        Decode.succeed (ResourceStopId (StopId id))

                    "trip" ->
                        Decode.succeed (ResourceTripId (TripId id))

                    "vehicle" ->
                        Decode.succeed (ResourceVehicleId (VehicleId id))

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

                    "stop" ->
                        Decode.map ResourceStop stopDecoder

                    "trip" ->
                        Decode.map ResourceTrip tripDecoder

                    "vehicle" ->
                        Decode.map ResourceVehicle vehicleDecoder

                    otherType ->
                        Decode.fail ("unrecognized type " ++ otherType)
            )


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


predictionIdDecoder : Decode.Decoder PredictionId
predictionIdDecoder =
    Decode.map PredictionId Decode.string


routeIdDecoder : Decode.Decoder RouteId
routeIdDecoder =
    Decode.map RouteId Decode.string


stopIdDecoder : Decode.Decoder StopId
stopIdDecoder =
    Decode.map StopId Decode.string


tripIdDecoder : Decode.Decoder TripId
tripIdDecoder =
    Decode.map TripId Decode.string


vehicleIdDecoder : Decode.Decoder VehicleId
vehicleIdDecoder =
    Decode.map VehicleId Decode.string


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
        |> Pipeline.optionalAt [ "relationships", "vehicle", "data", "id" ] (Decode.map Just vehicleIdDecoder) Nothing


stopDecoder : Decode.Decoder Stop
stopDecoder =
    checkType "stop" Stop
        |> Pipeline.required "id" stopIdDecoder
        |> Pipeline.requiredAt [ "attributes", "name" ] Decode.string
        |> Pipeline.optionalAt [ "relationships", "parent_station", "data", "id" ] (Decode.map Just stopIdDecoder) Nothing
        |> Pipeline.optionalAt [ "attributes", "platform_code" ] (Decode.map Just Decode.string) Nothing


tripDecoder : Decode.Decoder Trip
tripDecoder =
    checkType "trip" Trip
        |> Pipeline.required "id" tripIdDecoder
        |> Pipeline.requiredAt [ "attributes", "headsign" ] Decode.string


vehicleDecoder : Decode.Decoder Vehicle
vehicleDecoder =
    checkType "vehicle" Vehicle
        |> Pipeline.required "id" vehicleIdDecoder
        |> Pipeline.requiredAt [ "attributes", "label" ] Decode.string


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
