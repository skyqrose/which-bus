module Model exposing
    ( Model
    , Msg(..)
    , StopNames
    )

import Api.Stream
import Api.Types as Api
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
    , directionFormValue : Maybe Api.Direction
    , stopNames : StopNames
    , apiResult : Api.Stream.ApiResult
    }


type Msg
    = Tick Time.Posix
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | AddSelection Selection
    | TypeRouteId String
    | TypeStopId String
    | TypeDirection (Maybe Api.Direction)
    | ReceiveStopNames (Result Http.Error StopNames)
    | ApiMsg Api.Stream.Msg


type alias StopNames =
    Dict.Dict Api.StopId String
