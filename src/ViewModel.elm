module ViewModel exposing
    ( ShownPrediction
    , predictionsForSelection
    )

import Data exposing (Selection)
import Mbta
import Mbta.Api
import Time


type alias ShownPrediction =
    { time : Time.Posix
    , tripHeadsign : Maybe String
    , platformCode : Maybe String
    , vehicleLabel : Maybe String
    }


predictionsForSelection : Mbta.Api.Ok (List Mbta.Prediction) -> Selection -> List ShownPrediction
predictionsForSelection ok selection =
    let
        predictions : List Mbta.Prediction
        predictions =
            ok.data
    in
    predictions
        |> List.filter (predictionMatchesRouteId selection.routeId)
        |> List.filter (predictionMatchesStop ok.included selection.stopId)
        |> List.filter (predictionMatchesDirection selection.directionId)
        |> List.map
            (\prediction ->
                { time =
                    case ( prediction.arrivalTime, prediction.departureTime ) of
                        ( _, Just departureTime ) ->
                            departureTime

                        ( Just arrivalTime, _ ) ->
                            arrivalTime

                        ( Nothing, Nothing ) ->
                            Debug.todo "prediction missing arrival and departure times"
                , tripHeadsign =
                    ok.included
                        |> Mbta.Api.getIncludedTrip prediction.tripId
                        |> Maybe.map .headsign
                , platformCode =
                    ok.included
                        |> Mbta.Api.getIncludedStop prediction.stopId
                        |> Maybe.andThen .platformCode
                , vehicleLabel =
                    Maybe.map
                        (\vehicleId ->
                            Mbta.Api.getIncludedVehicle vehicleId ok.included
                                |> Maybe.map .label
                                |> Maybe.withDefault
                                    (case vehicleId of
                                        Mbta.VehicleId idString ->
                                            idString
                                    )
                        )
                        prediction.vehicleId
                }
            )


predictionMatchesRouteId : Mbta.RouteId -> Mbta.Prediction -> Bool
predictionMatchesRouteId routeId prediction =
    prediction.routeId == routeId


predictionMatchesStop : Mbta.Api.Included -> Mbta.StopId -> Mbta.Prediction -> Bool
predictionMatchesStop included queriedStop prediction =
    (prediction.stopId == queriedStop)
        || (let
                predictionParentStation =
                    included
                        |> Mbta.Api.getIncludedStop prediction.stopId
                        |> Maybe.andThen .parentStation
            in
            predictionParentStation == Just queriedStop
           )


predictionMatchesDirection : Maybe Mbta.DirectionId -> Mbta.Prediction -> Bool
predictionMatchesDirection selectionDirection prediction =
    case selectionDirection of
        Nothing ->
            True

        Just directionId ->
            prediction.directionId == directionId
