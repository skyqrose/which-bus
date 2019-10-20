module Data exposing (Selection)

import Mbta


type alias Selection =
    { routeIds : List Mbta.RouteId
    , stopId : Mbta.StopId
    , directionId : Maybe Mbta.DirectionId
    }
