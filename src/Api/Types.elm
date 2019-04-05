module Api.Types exposing
    ( DirectionId(..)
    , Prediction
    , PredictionId(..)
    , Resource(..)
    , ResourceId(..)
    , RouteId(..)
    , Stop
    , StopId(..)
    , Trip
    , TripId(..)
    , Vehicle
    , VehicleId(..)
    )

import Time


type ResourceId
    = ResourcePredictionId PredictionId
    | ResourceStopId StopId
    | ResourceTripId TripId
    | ResourceVehicleId VehicleId


type Resource
    = ResourcePrediction Prediction
    | ResourceStop Stop
    | ResourceTrip Trip
    | ResourceVehicle Vehicle


type DirectionId
    = D0
    | D1


type PredictionId
    = PredictionId String


type RouteId
    = RouteId String


type StopId
    = StopId String


type TripId
    = TripId String


type VehicleId
    = VehicleId String


type alias Prediction =
    { id : PredictionId
    , time : Time.Posix
    , routeId : RouteId
    , stopId : StopId
    , directionId : DirectionId
    , tripId : TripId
    , vehicleId : Maybe VehicleId
    }


type alias Stop =
    { id : StopId
    , name : String
    , parentStation : Maybe StopId
    , platformCode : Maybe String
    }


type alias Trip =
    { id : TripId
    , headsign : String
    }


type alias Vehicle =
    { id : VehicleId
    , label : String
    }
