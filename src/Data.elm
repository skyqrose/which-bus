module Data exposing
    ( Prediction
    , PredictionId(..)
    , RouteId(..)
    , Selection
    , StopId(..)
    )

import Time


type RouteId
    = RouteId String


type StopId
    = StopId String


type PredictionId
    = PredictionId String


type alias Selection =
    { routeId : RouteId
    , stopId : StopId
    }


type alias Prediction =
    { id : PredictionId
    , time : Time.Posix
    , selection : Selection
    }
