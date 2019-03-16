module Api.Types exposing
    ( Direction(..)
    , Prediction
    , PredictionId(..)
    , Resource(..)
    , ResourceId(..)
    , RouteId(..)
    , Stop
    , StopId(..)
    , Trip
    , TripId(..)
    )

import Time


type ResourceId
    = ResourcePredictionId PredictionId
    | ResourceTripId TripId
    | ResourceStopId StopId


type Resource
    = ResourcePrediction Prediction
    | ResourceTrip Trip
    | ResourceStop Stop


type PredictionId
    = PredictionId String


type RouteId
    = RouteId String


type StopId
    = StopId String


type Direction
    = Zero
    | One


type alias Prediction =
    { id : PredictionId
    , time : Time.Posix
    , routeId : RouteId
    , stopId : StopId
    , direction : Direction
    , tripId : TripId
    }


type TripId
    = TripId String


type alias Trip =
    { id : TripId
    , headsign : String
    }


type alias Stop =
    { id : StopId
    , parentStation : Maybe StopId
    }
