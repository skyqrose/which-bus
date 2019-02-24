port module Main exposing (main)

import AssocList as Dict
import Browser
import Browser.Navigation as Navigation
import Data exposing (..)
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
        selections =
            UrlParsing.parseSelectionsFromUrl url
    in
    ( { currentTime = Time.millisToPosix 0
      , url = url
      , navigationKey = key
      , selections = selections
      , predictionsData = Loading
      , routeIdFormText = ""
      , stopIdFormText = ""
      }
    , Cmd.batch
        [ Task.perform Tick Time.now
        , startStream selections
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
                newSelections =
                    UrlParsing.parseSelectionsFromUrl url
            in
            ( { model
                | url = url
                , selections = newSelections
                , predictionsData = Loading
              }
            , startStream newSelections
            )

        AddSelection newSelection ->
            let
                newSelections =
                    model.selections ++ [ newSelection ]
            in
            ( model
            , model.url
                |> UrlParsing.setSelectionsInUrl newSelections
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


startStream : List Selection -> Cmd Msg
startStream selections =
    let
        api_key =
            "3a6d67c08111426d8617a30340a9fad3"

        route_ids =
            selections
                |> List.map .routeId
                |> List.map (\(RouteId routeId) -> routeId)
                |> String.join ","

        stop_ids =
            selections
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

        ( Insert newPrediction, Success predictionsBySelection ) ->
            Success <|
                insertPrediction newPrediction predictionsBySelection

        ( Remove predictionId, Loading ) ->
            Loading

        ( Remove predictionId, Success predictionsBySelection ) ->
            -- We don't know which selection this prediction was for
            -- So we have to search all the selections for it.
            Success <|
                Dict.map
                    (\selection predictionsForSelection ->
                        Dict.remove predictionId predictionsForSelection
                    )
                    predictionsBySelection


insertPrediction : Prediction -> PredictionsBySelection -> PredictionsBySelection
insertPrediction prediction predictionsBySelection =
    Dict.update
        prediction.selection
        (\maybePredictionsForSelection ->
            case maybePredictionsForSelection of
                Nothing ->
                    Just (Dict.singleton prediction.id prediction)

                Just predictionsForSelection ->
                    Just (Dict.insert prediction.id prediction predictionsForSelection)
        )
        predictionsBySelection
