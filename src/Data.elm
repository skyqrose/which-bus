module Data exposing (Selection)

import Mbta


type alias Selection =
    { routeId : Mbta.RouteId
    , stopId : Mbta.StopId
    , directionId : Maybe Mbta.DirectionId
    }
