module Selection exposing
    ( Selection
    , isValid
    , selectedRouteIds
    , selectedStopIds
    )

import Maybe.Extra
import Mbta


type alias Selection =
    { routeIds : List Mbta.RouteId
    , stopId : Maybe Mbta.StopId
    , directionId : Maybe Mbta.DirectionId
    }


isValid : Selection -> Bool
isValid selection =
    not (List.isEmpty selection.routeIds) || Maybe.Extra.isJust selection.stopId


selectedRouteIds : List Selection -> List Mbta.RouteId
selectedRouteIds selections =
    List.concatMap .routeIds selections


selectedStopIds : List Selection -> List Mbta.StopId
selectedStopIds selections =
    List.filterMap .stopId selections
