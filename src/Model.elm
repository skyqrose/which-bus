module Model exposing
    ( Model
    , Msg(..)
    )

import AssocList exposing (Dict)
import Browser
import Browser.Navigation as Navigation
import Data exposing (Selection)
import Json.Decode as Decode
import Mbta
import Mbta.Api
import Time
import Url exposing (Url)


type alias Model =
    { currentTime : Maybe Time.Posix
    , url : Url
    , navigationKey : Navigation.Key
    , selections : List Selection
    , routeIdFormText : String
    , stopIdFormText : String
    , directionIdFormValue : Maybe Mbta.DirectionId
    , routes : Dict Mbta.RouteId Mbta.Route
    , stops : Dict Mbta.StopId Mbta.Stop
    , streamState : Mbta.Api.StreamState Mbta.Prediction
    , lastUpdated : Maybe Time.Posix
    }


type Msg
    = Tick Time.Posix
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | AddSelection Selection
    | TypeRouteId String
    | TypeStopId String
    | TypeDirection (Maybe Mbta.DirectionId)
    | DeleteSelection Int
    | ReceiveRoutes (Mbta.Api.ApiResult (List Mbta.Route))
    | ReceiveStops (Mbta.Api.ApiResult (List Mbta.Stop))
    | StreamMsg String Decode.Value
    | RefreshStream
