module Data exposing
    ( Direction(..)
    , RouteId(..)
    , Selection
    , ShownPrediction
    , StopId(..)
    )

import Time


type RouteId
    = RouteId String


type StopId
    = StopId String


type Direction
    = Zero
    | One


type alias Selection =
    { routeId : RouteId
    , stopId : StopId
    , direction : Maybe Direction
    }


type alias ShownPrediction =
    { time : Time.Posix
    , tripHeadsign : Maybe String
    }
