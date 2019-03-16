module Data exposing
    ( Selection
    , ShownPrediction
    )

import Api.Types as Api
import Time


type alias Selection =
    { routeId : Api.RouteId
    , stopId : Api.StopId
    , directionId : Maybe Api.DirectionId
    }


type alias ShownPrediction =
    { time : Time.Posix
    , tripHeadsign : Maybe String
    }
