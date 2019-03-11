module Model exposing
    ( Model
    , Msg(..)
    , StopNames
    )

import Api
import AssocList as Dict
import Browser
import Browser.Navigation as Navigation
import Data exposing (..)
import Http
import Json.Decode as Decode
import Time
import Url exposing (Url)


type alias Model =
    { currentTime : Time.Posix
    , url : Url
    , navigationKey : Navigation.Key
    , selections : List Selection
    , routeIdFormText : String
    , stopIdFormText : String
    , stopNames : StopNames
    , apiResult : Api.ApiResult
    }


type Msg
    = Tick Time.Posix
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | AddSelection Selection
    | TypeRouteId String
    | TypeStopId String
    | ReceiveStopNames (Result Http.Error StopNames)
    | ApiMsg Api.Msg


type alias StopNames =
    Dict.Dict StopId String
