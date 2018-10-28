module Model exposing
    ( Model
    , Msg(..)
    )

import Browser
import Url exposing (Url)


type alias Model =
    ()


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
