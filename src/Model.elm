module Model exposing
    ( Model
    , Msg(..)
    , Stop
    , encodeStop
    )

import Browser
import Browser.Navigation as Navigation
import Json.Encode
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


encodeStop : Stop -> Json.Encode.Value
encodeStop stop =
    Json.Encode.object
        [ ( "route_id", Json.Encode.string stop.routeId )
        , ( "stop_id", Json.Encode.string stop.stopId )
        ]
