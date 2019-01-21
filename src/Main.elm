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
    , startStream stops
    )


port startStreamPort : String -> Cmd msg


port streamEventPort : (Decode.Value -> msg) -> Sub msg


startStream : List Stop -> Cmd Msg
startStream stops =
    let
        api_key =
            "3a6d67c08111426d8617a30340a9fad3"

        route_ids =
            stops
                |> List.map .routeId
                |> String.join ","

        stop_ids =
            stops
                |> List.map .stopId
                |> String.join ","

        url =
            "https://api-v3.mbta.com/predictions"
                ++ "?api_key="
                ++ api_key
                ++ "&filter[route]="
                ++ route_ids
                ++ "&filter[stop]="
                ++ stop_ids
    in
    startStreamPort url


subscriptions : Model -> Sub Msg
subscriptions _ =
    streamEventPort
        (\json ->
            json
                |> Decode.decodeValue streamEventDecoder
                |> StreamEvent
        )
