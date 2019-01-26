port module Main exposing (main)

import AssocList as Dict
import Browser
import Browser.Navigation as Navigation
import Html
import Json.Decode as Decode
import Model exposing (..)
import Task
import Time
import Url exposing (Url)
import UrlParsing
import View exposing (view)


port startStreamPort : String -> Cmd msg


port streamEventPort : (Decode.Value -> msg) -> Sub msg


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
    ( { currentTime = Time.millisToPosix 0
      , url = url
      , navigationKey = key
      , stops = stops
      , predictionsData = Loading
      , routeIdFormText = ""
      , stopIdFormText = ""
      }
    , Cmd.batch
        [ Task.perform Tick Time.now
        , startStream stops
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model
                | currentTime = time
              }
            , Cmd.none
            )

        OnUrlRequest urlRequest ->
            ( model
            , Cmd.none
            )

        OnUrlChange url ->
            let
                newStops =
                    UrlParsing.parseStopsFromUrl url
            in
            ( { model
                | url = url
                , stops = newStops
                , predictionsData = Loading
              }
            , startStream newStops
            )

        AddStop newStop ->
            let
                newStops =
                    model.stops ++ [ newStop ]
            in
            ( model
            , model.url
                |> UrlParsing.setStopsInUrl newStops
                |> Url.toString
                |> Navigation.pushUrl model.navigationKey
            )

        TypeRouteId text ->
            ( { model
                | routeIdFormText = text
              }
            , Cmd.none
            )

        TypeStopId text ->
            ( { model
                | stopIdFormText = text
              }
            , Cmd.none
            )

        StreamEvent decodeResult ->
            case decodeResult of
                Ok event ->
                    let
                        _ =
                            Debug.log "successfully decoded" event
                    in
                    ( { model
                        | predictionsData = applyStreamEvent event model.predictionsData
                      }
                    , Cmd.none
                    )

                Err error ->
                    let
                        _ =
                            Debug.log "failed to decode" (Debug.toString error)
                    in
                    ( { model
                        | predictionsData = Failure error
                      }
                    , Cmd.none
                    )


startStream : List Stop -> Cmd Msg
startStream stops =
    let
        api_key =
            "3a6d67c08111426d8617a30340a9fad3"

        route_ids =
            stops
                |> List.map .routeId
                |> List.map (\(RouteId routeId) -> routeId)
                |> String.join ","

        stop_ids =
            stops
                |> List.map .stopId
                |> List.map (\(StopId stopId) -> stopId)
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
    Sub.batch
        [ Time.every 1000 Tick
        , streamEventPort
            (\json ->
                json
                    |> Decode.decodeValue streamEventDecoder
                    |> StreamEvent
            )
        ]


applyStreamEvent : StreamEvent -> PredictionsData -> PredictionsData
applyStreamEvent event predictionsData =
    case ( event, predictionsData ) of
        ( _, Failure error ) ->
            Failure error

        ( Reset newPredictions, _ ) ->
            Success <|
                List.foldl insertPrediction Dict.empty newPredictions

        ( Insert newPrediction, Loading ) ->
            Loading

        ( Insert newPrediction, Success predictionsByStop ) ->
            Success <|
                insertPrediction newPrediction predictionsByStop

        ( Remove predictionId, Loading ) ->
            Loading

        ( Remove predictionId, Success predictionsByStop ) ->
            -- We don't know which stop this prediction was for
            -- So we have to search all the stops for it.
            Success <|
                Dict.map
                    (\stop predictionsForStop ->
                        Dict.remove predictionId predictionsForStop
                    )
                    predictionsByStop


insertPrediction : Prediction -> PredictionsByStop -> PredictionsByStop
insertPrediction prediction predictionsByStop =
    Dict.update
        prediction.stop
        (\maybePredictionsForStop ->
            case maybePredictionsForStop of
                Nothing ->
                    Just (Dict.singleton prediction.id prediction)

                Just predictionsForStop ->
                    Just (Dict.insert prediction.id prediction predictionsForStop)
        )
        predictionsByStop
