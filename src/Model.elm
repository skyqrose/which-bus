module Model exposing
    ( Model
    , Msg(..)
    , Stop
    )

import Browser
import Url exposing (Url)


type alias Model =
    { stops : List Stop
    }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url


type alias Stop =
    { routeId : String
    , stopId : String
    }
