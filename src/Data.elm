module Data exposing
    ( RouteId(..)
    , Selection
    , ShownPrediction
    , StopId(..)
    )

import Time


type RouteId
    = RouteId String


type StopId
    = StopId String


type alias Selection =
    { routeId : RouteId
    , stopId : StopId
    }


type alias ShownPrediction =
    { time : Time.Posix
    , tripHeadsign : Maybe String
    }
