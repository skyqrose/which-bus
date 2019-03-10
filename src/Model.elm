module Model exposing
    ( Model
    , Msg(..)
    )

import Api exposing (..)
import Browser
import Browser.Navigation as Navigation
import Data exposing (..)
import Json.Decode as Decode
import Time
import Url exposing (Url)


type alias Model =
    { currentTime : Time.Posix
    , url : Url
    , navigationKey : Navigation.Key
    , selections : List Selection
    , predictionsData : PredictionsData
    , routeIdFormText : String
    , stopIdFormText : String
    }


type Msg
    = Tick Time.Posix
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | AddSelection Selection
    | TypeRouteId String
    | TypeStopId String
    | StreamEvent (Result Decode.Error StreamEvent)
