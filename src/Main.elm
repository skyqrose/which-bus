module Main exposing (main)

import Browser.Navigation as Navigation
import Browser
import Html
import Json.Decode
import Model exposing (..)
import Update exposing (update)
import Url exposing (Url)
import UrlParsing
import View exposing (view)


main : Program Json.Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : Json.Decode.Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { url = url
    , navigationKey = key
    , stops = UrlParsing.parseStopsFromUrl url
    , routeIdFormText = ""
    , stopIdFormText = ""
    }
    , Cmd.none
    )
