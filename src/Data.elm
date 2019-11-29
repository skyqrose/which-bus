module Data exposing
    ( Selection
    , selectedRouteIds
    , selectedStopIds
    )

import Mbta


type alias Selection =
    { routeIds : List Mbta.RouteId
    , stopId : Maybe Mbta.StopId
    , directionId : Maybe Mbta.DirectionId
    }


selectedRouteIds : List Selection -> List Mbta.RouteId
selectedRouteIds selections =
    List.concatMap .routeIds selections


selectedStopIds : List Selection -> List Mbta.StopId
selectedStopIds selections =
    List.filterMap .stopId selections
