port module Main exposing (main)

import AssocList as Dict
import Browser
import Browser.Navigation as Navigation
import Data exposing (Selection)
import Json.Decode as Decode exposing (Decoder)
import Mbta
import Mbta.Api
import Model exposing (..)
import Task
import Time
import Url exposing (Url)
import UrlParsing
import View exposing (view)


{-| Takes a url
-}
port startStream : String -> Cmd msg


port streamEventPort : ({ eventName : String, data : Decode.Value } -> msg) -> Sub msg


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

        ( initStreamState, streamUrl ) =
            streamPredictions selections
    in
    ( { currentTime = Time.millisToPosix 0
      , url = url
      , navigationKey = key
      , selections = selections
      , routeIdFormText = ""
      , stopIdFormText = ""
      , directionIdFormValue = Nothing
      , routes = Dict.empty
      , stops = Dict.empty
      , streamState = initStreamState
      , lastUpdated = Nothing
      }
    , Cmd.batch
        [ Task.perform Tick Time.now
        , startStream streamUrl
        , getRoutes selections
        , getStops selections
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

                ( initStreamState, streamUrl ) =
                    streamPredictions newSelections
            in
            ( { model
                | url = url
                , selections = newSelections
                , streamState = initStreamState
              }
            , Cmd.batch
                [ startStream streamUrl
                , getRoutes newSelections
                , getStops newSelections
                ]
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

        TypeDirection directionId ->
            ( { model
                | directionIdFormValue = directionId
              }
            , Cmd.none
            )

        ReceiveRoutes apiResult ->
            ( { model
                | routes =
                    apiResult
                        |> Result.map Mbta.Api.getPrimaryData
                        |> Result.withDefault []
                        |> List.map (\route -> ( route.id, route ))
                        |> Dict.fromList
              }
            , Cmd.none
            )

        ReceiveStops apiResult ->
            ( { model
                | stops =
                    apiResult
                        |> Result.map Mbta.Api.getPrimaryData
                        |> Result.withDefault []
                        |> List.map (\stop -> ( Mbta.stopId stop, stop ))
                        |> Dict.fromList
              }
            , Cmd.none
            )

        StreamMsg eventName dataJson ->
            ( { model
                | streamState = Mbta.Api.updateStream eventName dataJson model.streamState
                , lastUpdated = Just model.currentTime
              }
            , Cmd.none
            )

        RefreshStream ->
            let
                ( initStreamState, streamUrl ) =
                    streamPredictions model.selections
            in
            ( { model
                | streamState = initStreamState
              }
            , startStream streamUrl
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , streamEventSub
        ]


apiHost : Mbta.Api.Host
apiHost =
    Mbta.Api.Default
        { apiKey = Just "3a6d67c08111426d8617a30340a9fad3" }


getRoutes : List Selection -> Cmd Msg
getRoutes selections =
    Mbta.Api.getRoutes
        ReceiveRoutes
        apiHost
        []
        [ Mbta.Api.filterRoutesByIds (List.map .routeId selections) ]


getStops : List Selection -> Cmd Msg
getStops selections =
    Mbta.Api.getStops
        ReceiveStops
        apiHost
        []
        [ Mbta.Api.filterStopsByIds (List.map .stopId selections) ]


streamPredictions : List Selection -> ( Mbta.Api.StreamState Mbta.Prediction, String )
streamPredictions selections =
    let
        routeIds : List Mbta.RouteId
        routeIds =
            List.map .routeId selections

        stopIds : List Mbta.StopId
        stopIds =
            List.map .stopId selections
    in
    Mbta.Api.streamPredictions
        apiHost
        [ Mbta.Api.include Mbta.Api.predictionStop
        , Mbta.Api.include Mbta.Api.predictionTrip
        , Mbta.Api.include Mbta.Api.predictionVehicle
        ]
        [ Mbta.Api.filterPredictionsByRouteIds routeIds
        , Mbta.Api.filterPredictionsByStopIds stopIds
        ]


streamEventSub : Sub Msg
streamEventSub =
    streamEventPort (\{ eventName, data } -> StreamMsg eventName data)
