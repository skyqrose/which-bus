module Model exposing
    ( Model
    , Msg(..)
    , Stop
    )

import Browser
import Browser.Navigation as Navigation
import Url exposing (Url)


type alias Model =
    { url : Url
    , navigationKey : Navigation.Key
    , stops : List Stop
    , routeIdFormText : String
    , stopIdFormText : String
    }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | AddStop Stop
    | TypeRouteId String
    | TypeStopId String


type alias Stop =
    { routeId : String
    , stopId : String
    }
