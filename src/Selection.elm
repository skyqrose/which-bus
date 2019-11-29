module Selection exposing
    ( Selection
    , addRouteId
    , removeRouteId
    , setDirectionId
    )

import List.Extra
import Mbta


type alias Selection =
    { routeIds : List Mbta.RouteId
    , stopId : Mbta.StopId
    , directionId : Maybe Mbta.DirectionId
    }


removeRouteId : Mbta.RouteId -> Selection -> Selection
removeRouteId routeId selection =
    { selection
        | routeIds =
            List.Extra.remove
                routeId
                selection.routeIds
    }


addRouteId : Mbta.RouteId -> Selection -> Selection
addRouteId routeId selection =
    { selection
        | routeIds =
            List.append
                selection.routeIds
                [ routeId ]
    }


setDirectionId : Maybe Mbta.DirectionId -> Selection -> Selection
setDirectionId newDirectionId selection =
    { selection
        | directionId =
            newDirectionId
    }
