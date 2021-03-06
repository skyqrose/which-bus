module ViewModel exposing
    ( ShownPrediction
    , predictionsForSelection
    )

import Color
import Maybe.Extra
import Mbta
import Mbta.Api
import Mbta.Extra
import Selection exposing (Selection)
import Time


type alias ShownPrediction =
    { time : Time.Posix
    , routeName : String
    , tripHeadsign : Maybe String
    , tripName : Maybe String
    , platformCode : Maybe String
    , vehicleLabel : Maybe String
    , scheduledTime : Maybe Time.Posix
    , backgroundColor : Color.Color
    , textColor : Color.Color
    }


predictionsForSelection : Mbta.Api.Data (List Mbta.Prediction) -> Selection -> List ShownPrediction
predictionsForSelection data selection =
    let
        predictions : List Mbta.Prediction
        predictions =
            Mbta.Api.getPrimaryData data
                |> List.filter (predictionMatchesRouteId selection.routeIds)
                |> List.filter (predictionMatchesStop data selection.stopId)
                |> List.filter (predictionMatchesDirection selection.directionId)
    in
    predictions
        |> List.filterMap
            (\prediction ->
                let
                    route : Maybe Mbta.Route
                    route =
                        Mbta.Api.getIncludedRoute prediction.routeId data

                    trip : Maybe Mbta.Trip
                    trip =
                        Mbta.Api.getIncludedTrip prediction.tripId data

                    shouldShowSchedule : Bool
                    shouldShowSchedule =
                        case route of
                            Nothing ->
                                True

                            Just { routeType } ->
                                List.member
                                    routeType
                                    [ Mbta.RouteType_2_CommuterRail, Mbta.RouteType_3_Bus, Mbta.RouteType_4_Ferry ]

                    maybeTime : Maybe Time.Posix
                    maybeTime =
                        case ( prediction.arrivalTime, prediction.departureTime ) of
                            ( _, Just departureTime ) ->
                                Just departureTime

                            ( Just arrivalTime, _ ) ->
                                Just arrivalTime

                            ( Nothing, Nothing ) ->
                                Nothing
                in
                Maybe.map
                    (\time ->
                        { time = time
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
                            trip
                                |> Maybe.map .headsign
                        , tripName =
                            trip
                                |> Maybe.andThen .name
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
                        , scheduledTime =
                            if shouldShowSchedule then
                                prediction.scheduleId
                                    |> Maybe.andThen
                                        (\scheduleId ->
                                            Mbta.Api.getIncludedSchedule scheduleId data
                                        )
                                    |> Maybe.andThen
                                        (\schedule ->
                                            Maybe.Extra.or schedule.departureTime schedule.arrivalTime
                                        )

                            else
                                Nothing
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
                    maybeTime
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
