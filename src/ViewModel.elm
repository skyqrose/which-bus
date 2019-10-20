module ViewModel exposing
    ( ShownPrediction
    , predictionsForSelection
    )

import Color
import Data exposing (Selection)
import Maybe.Extra
import Mbta
import Mbta.Api
import Mbta.Extra
import Time


type alias ShownPrediction =
    { time : Time.Posix
    , routeName : String
    , tripHeadsign : Maybe String
    , platformCode : Maybe String
    , vehicleLabel : Maybe String
    , backgroundColor : Color.Color
    , textColor : Color.Color
    }


predictionsForSelection : Mbta.Api.Data (List Mbta.Prediction) -> Selection -> List ShownPrediction
predictionsForSelection data selection =
    let
        predictions : List Mbta.Prediction
        predictions =
            Mbta.Api.getPrimaryData data
    in
    predictions
        |> List.filter (predictionMatchesRouteId selection.routeIds)
        |> List.filter (predictionMatchesStop data selection.stopId)
        |> List.filter (predictionMatchesDirection selection.directionId)
        |> List.map
            (\prediction ->
                let
                    route : Maybe Mbta.Route
                    route =
                        Mbta.Api.getIncludedRoute prediction.routeId data
                in
                { time =
                    case ( prediction.arrivalTime, prediction.departureTime ) of
                        ( _, Just departureTime ) ->
                            departureTime

                        ( Just arrivalTime, _ ) ->
                            arrivalTime

                        ( Nothing, Nothing ) ->
                            Debug.todo "prediction missing arrival and departure times"
                , routeName =
                    route
                        |> Maybe.andThen Mbta.Extra.routeAbbreviation
                        |> Maybe.Extra.orElse (Maybe.map .longName route)
                        |> Maybe.withDefault
                            (case prediction.routeId of
                                Mbta.RouteId routeId ->
                                    routeId
                            )
                , tripHeadsign =
                    data
                        |> Mbta.Api.getIncludedTrip prediction.tripId
                        |> Maybe.map .headsign
                , platformCode =
                    data
                        |> Mbta.Api.getIncludedStopStop prediction.stopId
                        |> Maybe.andThen .platformCode
                , vehicleLabel =
                    Maybe.map
                        (\vehicleId ->
                            data
                                |> Mbta.Api.getIncludedVehicle vehicleId
                                |> Maybe.map .label
                                |> Maybe.withDefault
                                    (case vehicleId of
                                        Mbta.VehicleId idString ->
                                            idString
                                    )
                        )
                        prediction.vehicleId
                , backgroundColor =
                    route
                        |> Maybe.map .color
                        |> Maybe.withDefault Color.white
                , textColor =
                    route
                        |> Maybe.map .textColor
                        |> Maybe.withDefault Color.black
                }
            )


predictionMatchesRouteId : List Mbta.RouteId -> Mbta.Prediction -> Bool
predictionMatchesRouteId routeIds prediction =
    List.member prediction.routeId routeIds


predictionMatchesStop : Mbta.Api.Data primary -> Mbta.StopId -> Mbta.Prediction -> Bool
predictionMatchesStop dataWithStopsIncluded queriedStop prediction =
    (prediction.stopId == queriedStop)
        || (let
                predictionParentStation =
                    dataWithStopsIncluded
                        |> Mbta.Api.getIncludedStopStop prediction.stopId
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
