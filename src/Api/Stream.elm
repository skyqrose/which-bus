port module Api.Stream exposing
    ( ApiData
    , ApiResult(..)
    , Error(..)
    , Msg
    , init
    , predictionsForSelection
    , subscriptions
    , update
    )

import Api.Decoders
import Api.Types exposing (..)
import Api.Url
import AssocList as Dict
import Data exposing (..)
import Json.Decode as Decode
import Json.Encode
import Time


port startStreamPort : String -> Cmd msg


port streamEventPort : (Decode.Value -> msg) -> Sub msg


type ApiResult
    = Loading
    | Failure Error
    | Success
        { lastUpdated : Time.Posix
        , apiData : ApiData
        }


type Error
    = DecodeError Decode.Error
    | BadOrder String


type alias ApiData =
    { predictions : Dict.Dict PredictionId Prediction
    , stops : Dict.Dict StopId Stop
    , trips : Dict.Dict TripId Trip
    , vehicles : Dict.Dict VehicleId Vehicle
    }


type alias Msg =
    Result Decode.Error StreamEvent


type StreamEvent
    = Reset (List Resource)
    | Insert Resource
    | Remove ResourceId


init : List Selection -> ( ApiResult, Cmd msg )
init selections =
    ( Loading
    , startStream selections
    )


emptyData : ApiData
emptyData =
    { predictions = Dict.empty
    , stops = Dict.empty
    , trips = Dict.empty
    , vehicles = Dict.empty
    }


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
            Api.Url.url
                "predictions"
                [ ( "filter[route]", routeIds )
                , ( "filter[stop]", stopIds )
                , ( "include", "stop,trip,vehicle" )
                ]
    in
    startStreamPort url


subscriptions : (Msg -> msg) -> Sub msg
subscriptions msg =
    streamEventPort (Decode.decodeValue streamEventDecoder >> msg)


update : Time.Posix -> Msg -> ApiResult -> ApiResult
update currentTime eventDecodeResult apiResult =
    case ( eventDecodeResult, apiResult ) of
        ( _, Failure error ) ->
            Failure error

        ( Err decodeError, _ ) ->
            Failure (DecodeError decodeError)

        ( Ok (Reset newResources), _ ) ->
            Success
                { lastUpdated = currentTime
                , apiData = List.foldl insertResource emptyData newResources
                }

        ( Ok (Insert _), Loading ) ->
            Failure (BadOrder "Insert while Loading")

        ( Ok (Remove _), Loading ) ->
            Failure (BadOrder "Remove while Loading")

        ( Ok (Insert newResource), Success { apiData } ) ->
            Success
                { lastUpdated = currentTime
                , apiData = insertResource newResource apiData
                }

        ( Ok (Remove resourceId), Success { apiData } ) ->
            case removeResource resourceId apiData of
                Nothing ->
                    Failure
                        (BadOrder
                            ("Remove nonexistent resource id " ++ Debug.toString resourceId)
                        )

                Just updatedApiData ->
                    Success
                        { lastUpdated = currentTime
                        , apiData = updatedApiData
                        }


insertResource : Resource -> ApiData -> ApiData
insertResource resource apiData =
    case resource of
        ResourcePrediction prediction ->
            { apiData
                | predictions =
                    Dict.insert prediction.id prediction apiData.predictions
            }

        ResourceStop stop ->
            { apiData
                | stops =
                    Dict.insert stop.id stop apiData.stops
            }

        ResourceTrip trip ->
            { apiData
                | trips =
                    Dict.insert trip.id trip apiData.trips
            }

        ResourceVehicle vehicle ->
            { apiData
                | vehicles =
                    Dict.insert vehicle.id vehicle apiData.vehicles
            }


removeResource : ResourceId -> ApiData -> Maybe ApiData
removeResource resourceId apiData =
    case resourceId of
        ResourcePredictionId predictionId ->
            if Dict.member predictionId apiData.predictions then
                Just
                    { apiData
                        | predictions =
                            Dict.remove predictionId apiData.predictions
                    }

            else
                Nothing

        ResourceStopId stopId ->
            if Dict.member stopId apiData.stops then
                Just
                    { apiData
                        | stops =
                            Dict.remove stopId apiData.stops
                    }

            else
                Nothing

        ResourceTripId tripId ->
            if Dict.member tripId apiData.trips then
                Just
                    { apiData
                        | trips =
                            Dict.remove tripId apiData.trips
                    }

            else
                Nothing

        ResourceVehicleId vehicleId ->
            if Dict.member vehicleId apiData.vehicles then
                Just
                    { apiData
                        | vehicles =
                            Dict.remove vehicleId apiData.vehicles
                    }

            else
                Nothing


predictionsForSelection : ApiData -> Selection -> List ShownPrediction
predictionsForSelection apiData selection =
    apiData.predictions
        |> Dict.values
        |> List.filter (\prediction -> prediction.routeId == selection.routeId)
        |> List.filter (predictionMatchesStop apiData selection.stopId)
        |> List.filter (predictionMatchesDirection selection.directionId)
        |> List.map
            (\prediction ->
                { time = prediction.time
                , tripHeadsign =
                    apiData.trips
                        |> Dict.get prediction.tripId
                        |> Maybe.map .headsign
                , platformCode =
                    apiData.stops
                        |> Dict.get prediction.stopId
                        |> Maybe.andThen .platformCode
                , vehicleLabel =
                    prediction.vehicleId
                        |> Maybe.map
                            (\(VehicleId vehicleId) ->
                                Dict.get (VehicleId vehicleId) apiData.vehicles
                                    |> Maybe.map .label
                                    |> Maybe.withDefault vehicleId
                            )
                }
            )


predictionMatchesStop : ApiData -> StopId -> Prediction -> Bool
predictionMatchesStop apiData queriedStop prediction =
    (prediction.stopId == queriedStop)
        || (let
                predictionParentStation =
                    Dict.get prediction.stopId apiData.stops
                        |> Maybe.andThen .parentStation
            in
            predictionParentStation == Just queriedStop
           )


predictionMatchesDirection : Maybe DirectionId -> Prediction -> Bool
predictionMatchesDirection selectionDirection prediction =
    case selectionDirection of
        Nothing ->
            True

        Just directionId ->
            prediction.directionId == directionId



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
            Decode.map Reset (Decode.list Api.Decoders.resourceDecoder)

        "add" ->
            Decode.map Insert Api.Decoders.resourceDecoder

        "update" ->
            Decode.map Insert Api.Decoders.resourceDecoder

        "remove" ->
            Decode.map Remove Api.Decoders.resourceIdDecoder

        _ ->
            Decode.fail ("unrecognized event name " ++ eventName)
