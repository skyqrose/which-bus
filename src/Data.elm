module Data exposing
    ( Selection
    , ShownPrediction
    )

import Api.Types as Api
import Time


type alias Selection =
    { routeId : Api.RouteId
    , stopId : Api.StopId
    , direction : Maybe Api.Direction
    }


type alias ShownPrediction =
    { time : Time.Posix
    , tripHeadsign : Maybe String
    }
