module Selection exposing
    ( CompleteSelection
    , PartialSelection
    , Selection(..)
    , addRouteId
    , directionId
    , filter
    , isValid
    , removeRouteId
    , routeIds
    , setDirectionId
    , stopId
    )

import List.Extra
import Mbta


type alias CompleteSelection =
    { routeIds : List Mbta.RouteId
    , stopId : Mbta.StopId
    , directionId : Maybe Mbta.DirectionId
    }


type alias PartialSelection =
    { routeIds : List Mbta.RouteId
    , directionId : Maybe Mbta.DirectionId
    }


type Selection
    = WithoutStop PartialSelection
    | WithStop CompleteSelection


isValid : Selection -> Bool
isValid selection =
    case selection of
        WithoutStop _ ->
            True

        WithStop completeSelection ->
            not (List.isEmpty completeSelection.routeIds)


routeIds : Selection -> List Mbta.RouteId
routeIds selection =
    case selection of
        WithoutStop partialSelection ->
            partialSelection.routeIds

        WithStop completeSelection ->
            completeSelection.routeIds


stopId : Selection -> Maybe Mbta.StopId
stopId selection =
    case selection of
        WithoutStop _ ->
            Nothing

        WithStop completeSelection ->
            Just completeSelection.stopId


directionId : Selection -> Maybe Mbta.DirectionId
directionId selection =
    case selection of
        WithoutStop partialSelection ->
            partialSelection.directionId

        WithStop completeSelection ->
            completeSelection.directionId


filter : List Selection -> List CompleteSelection
filter maybeSelections =
    List.filterMap
        (\selection ->
            case selection of
                WithoutStop _ ->
                    Nothing

                WithStop completeSelection ->
                    Just completeSelection
        )
        maybeSelections


removeRouteId : Mbta.RouteId -> Selection -> Selection
removeRouteId routeId selection =
    case selection of
        WithoutStop partialSelection ->
            WithoutStop { partialSelection | routeIds = List.Extra.remove routeId partialSelection.routeIds }

        WithStop completeSelection ->
            WithStop { completeSelection | routeIds = List.Extra.remove routeId completeSelection.routeIds }


addRouteId : Mbta.RouteId -> Selection -> Selection
addRouteId routeId selection =
    case selection of
        WithoutStop partialSelection ->
            WithoutStop { partialSelection | routeIds = List.append partialSelection.routeIds [ routeId ] }

        WithStop completeSelection ->
            WithStop { completeSelection | routeIds = List.append completeSelection.routeIds [ routeId ] }


setDirectionId : Maybe Mbta.DirectionId -> Selection -> Selection
setDirectionId newDirectionId selection =
    case selection of
        WithoutStop partialSelection ->
            WithoutStop { partialSelection | directionId = newDirectionId }

        WithStop completeSelection ->
            WithStop { completeSelection | directionId = newDirectionId }
