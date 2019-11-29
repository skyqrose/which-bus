module Model exposing
    ( Modal(..)
    , Model
    , Msg(..)
    )

import AssocList exposing (Dict)
import Browser
import Browser.Navigation as Navigation
import Json.Decode as Decode
import Mbta
import Mbta.Api
import Selection exposing (Selection)
import Time
import Url exposing (Url)


type alias Model =
    -- bookkeeping
    { currentTime : Maybe Time.Posix
    , url : Url
    , navigationKey : Navigation.Key

    --ui
    , selections : List Selection
    , routeIdFormText : String
    , stopIdFormText : String
    , modal : Modal

    -- data
    , routes : List Mbta.Route
    , stops : Dict Mbta.StopId Mbta.Stop
    , routesByStopId : Dict Mbta.StopId (List Mbta.Route)
    , streamState : Mbta.Api.StreamState Mbta.Prediction
    , lastUpdated : Maybe Time.Posix
    }


type
    Msg
    -- bookkeeping
    = Tick Time.Posix
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
      -- ui
    | AddSelection Selection
    | TypeRouteId String
    | TypeStopId String
    | DeleteSelection Int
    | ToggleDirection Int
    | AddRouteToSelection Int Mbta.RouteId
    | CloseModal
    | OpenRoutePicker (Maybe Int)
    | PickRoute Mbta.RouteId
      -- data
    | RemoveRouteFromSelection Int Mbta.RouteId
    | ReceiveRoutes (Mbta.Api.ApiResult (List Mbta.Route))
    | ReceiveStops (Mbta.Api.ApiResult (List Mbta.Stop))
    | ReceiveRoutesForStopId Mbta.StopId (Mbta.Api.ApiResult (List Mbta.Route))
    | StreamMsg String Decode.Value
    | RefreshStream


type Modal
    = NoModal
      -- Index for which selection to add to. Nothing means make a new selection.
    | RoutePicker (Maybe Int)
