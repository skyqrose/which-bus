port module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html
import Json.Decode as Decode
import Model exposing (..)
import Update exposing (update)
import Url exposing (Url)
import UrlParsing
import View exposing (view)


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : Decode.Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        stops =
            UrlParsing.parseStopsFromUrl url
    in
    ( { url = url
      , navigationKey = key
      , stops = stops
      , routeIdFormText = ""
      , stopIdFormText = ""
      }
    , Cmd.batch (List.map predictionStreamStop stops)
    )


port predictionStreamJson : Decode.Value -> Cmd msg


port predictionEvent : (Decode.Value -> msg) -> Sub msg


predictionStreamStop : Stop -> Cmd Msg
predictionStreamStop stop =
    stop
        |> encodeStop
        |> predictionStreamJson


subscriptions : Model -> Sub Msg
subscriptions _ =
    predictionEvent PredictionEvent
