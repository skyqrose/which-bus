module Model exposing
    ( Model
    , Msg(..)
    , NewSelectionState(..)
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
    , newSelectionState : NewSelectionState

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
    | DeleteSelection Int
    | ToggleDirection Int
    | AddRouteToSelection Int Mbta.RouteId
    | NewSelectionStart
    | NewSelectionChoseRoute Mbta.RouteId
    | NewSelectionChoseDirection (Maybe Mbta.DirectionId)
    | NewSelectionChoseStop Mbta.StopId
    | NewSelectionAddExtraRoute
    | NewSelectionRemoveRoute Mbta.RouteId
    | NewSelectionBack
    | NewSelectionCancel
      -- data
    | RemoveRouteFromSelection Int Mbta.RouteId
    | ReceiveRoutes (Mbta.Api.ApiResult (List Mbta.Route))
    | ReceiveStops (Mbta.Api.ApiResult (List Mbta.Stop))
    | ReceiveRoutesForStopId Mbta.StopId (Mbta.Api.ApiResult (List Mbta.Route))
    | ReceiveStopsForRoutes Mbta.RouteId (Maybe Mbta.DirectionId) (Mbta.Api.ApiResult (List Mbta.Stop))
    | StreamMsg String Decode.Value
    | RefreshStream


type NewSelectionState
    = NotMakingNewSelection
    | ChoosingRoute
    | ChoosingStop (List Mbta.RouteId) (Maybe Mbta.DirectionId) (Dict Mbta.RouteId (List Mbta.Stop))
    | ChoosingExtraRoutes (List Mbta.RouteId) (Maybe Mbta.DirectionId)
