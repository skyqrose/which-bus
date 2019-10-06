module Model exposing
    ( Model
    , Msg(..)
    , StopNames
    )

import AssocList as Dict
import Browser
import Browser.Navigation as Navigation
import Data exposing (Selection)
import Http
import Json.Decode as Decode
import Mbta
import Mbta.Api
import Time
import Url exposing (Url)


type alias Model =
    { currentTime : Time.Posix
    , url : Url
    , navigationKey : Navigation.Key
    , selections : List Selection
    , routeIdFormText : String
    , stopIdFormText : String
    , directionIdFormValue : Maybe Mbta.DirectionId
    , stopNames : StopNames
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
    | ReceiveStopNames (Mbta.Api.ApiResult (List Mbta.Stop))
    | StreamMsg String Decode.Value
    | RefreshStream


type alias StopNames =
    Dict.Dict Mbta.StopId String
