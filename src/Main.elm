port module Main exposing (main)

import AssocList as Dict exposing (Dict)
import Browser
import Browser.Navigation as Navigation
import Json.Decode as Decode
import List.Extra
import Mbta
import Mbta.Api
import Model exposing (..)
import Selection exposing (Selection)
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
    ( { currentTime = Nothing
      , url = url
      , navigationKey = key
      , selections = selections
      , routeIdFormText = ""
      , stopIdFormText = ""
      , routes = Dict.empty
      , stops = Dict.empty
      , routesByStopId = Dict.empty
      , streamState = initStreamState
      , lastUpdated = Nothing
      }
    , Cmd.batch
        [ Task.perform Tick Time.now
        , getRoutes
        , getStops selections
        , getRoutesByStopId Dict.empty selections
        , startStream streamUrl
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                newModel =
                    { model
                        | currentTime = Just time
                    }
            in
            if restartStream newModel then
                let
                    ( initStreamState, streamUrl ) =
                        streamPredictions model.selections
                in
                ( { newModel
                    | streamState = initStreamState
                    , lastUpdated = Nothing
                  }
                , startStream streamUrl
                )

            else
                ( newModel
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

                -- ingore the new stream state
                -- so we keep looking at the old data
                -- until the new one sends its first reset
                ( _, streamUrl ) =
                    streamPredictions newSelections
            in
            ( { model
                | url = url
                , selections = newSelections
              }
            , Cmd.batch
                [ getStops newSelections
                , getRoutesByStopId model.routesByStopId newSelections
                , startStream streamUrl
                ]
            )

        AddSelection newSelection ->
            let
                newSelections =
                    model.selections ++ [ newSelection ]
            in
            registerNewSelections model newSelections

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

        DeleteSelection index ->
            let
                newSelections =
                    List.Extra.removeAt index model.selections
            in
            registerNewSelections model newSelections

        ToggleDirection index ->
            let
                newSelections =
                    List.Extra.updateAt
                        index
                        (\selection ->
                            { selection
                                | directionId =
                                    case selection.directionId of
                                        Nothing ->
                                            Just Mbta.D1

                                        Just Mbta.D1 ->
                                            Just Mbta.D0

                                        Just Mbta.D0 ->
                                            Nothing
                            }
                        )
                        model.selections
            in
            registerNewSelections model newSelections

        AddRouteToSelection index routeId ->
            let
                newSelections =
                    List.Extra.updateAt
                        index
                        (\selection ->
                            { selection
                                | routeIds =
                                    routeId :: selection.routeIds
                            }
                        )
                        model.selections
            in
            registerNewSelections model newSelections

        RemoveRouteFromSelection index routeId ->
            let
                newSelections =
                    model.selections
                        |> List.Extra.updateAt
                            index
                            (\selection ->
                                { selection
                                    | routeIds =
                                        List.Extra.remove routeId selection.routeIds
                                }
                            )
                        |> List.filter Selection.isValid
            in
            registerNewSelections model newSelections

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

        ReceiveRoutesForStopId stopId apiResult ->
            ( { model
                | routesByStopId =
                    Dict.insert
                        stopId
                        (apiResult
                            |> Result.map Mbta.Api.getPrimaryData
                            |> Result.withDefault []
                        )
                        model.routesByStopId
              }
            , Cmd.none
            )

        StreamMsg eventName dataJson ->
            ( { model
                | streamState = Mbta.Api.updateStream eventName dataJson model.streamState
                , lastUpdated = model.currentTime
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
                , lastUpdated = Nothing
              }
            , startStream streamUrl
            )


registerNewSelections : Model -> List Selection -> ( Model, Cmd Msg )
registerNewSelections model newSelections =
    ( model
    , model.url
        |> UrlParsing.setSelectionsInUrl newSelections
        |> Url.toString
        |> Navigation.pushUrl model.navigationKey
    )


restartStream : Model -> Bool
restartStream model =
    case ( model.currentTime, model.lastUpdated ) of
        ( Just currentTime, Just lastUpdated ) ->
            (Time.posixToMillis currentTime - Time.posixToMillis lastUpdated) // 1000 > 60

        _ ->
            False


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


getRoutes : Cmd Msg
getRoutes =
    Mbta.Api.getRoutes
        ReceiveRoutes
        apiHost
        []
        []


getStops : List Selection -> Cmd Msg
getStops selections =
    Mbta.Api.getStops
        ReceiveStops
        apiHost
        []
        [ Mbta.Api.filterStopsByIds (Selection.selectedStopIds selections) ]


getRoutesByStopId : Dict Mbta.StopId (List Mbta.Route) -> List Selection -> Cmd Msg
getRoutesByStopId existingRoutesByStopId selections =
    Cmd.batch
        (List.map
            (\stopId ->
                case Dict.get stopId existingRoutesByStopId of
                    Just (_ :: _) ->
                        -- Already have a result, don't need to refetch
                        Cmd.none

                    _ ->
                        Mbta.Api.getRoutes
                            (ReceiveRoutesForStopId stopId)
                            apiHost
                            []
                            [ Mbta.Api.filterRoutesByStopIds [ stopId ]
                            ]
            )
            (Selection.selectedStopIds selections)
        )


streamPredictions : List Selection -> ( Mbta.Api.StreamState Mbta.Prediction, String )
streamPredictions selections =
    Mbta.Api.streamPredictions
        apiHost
        [ Mbta.Api.include Mbta.Api.predictionStop
        , Mbta.Api.include Mbta.Api.predictionTrip
        , Mbta.Api.include Mbta.Api.predictionVehicle
        , Mbta.Api.include Mbta.Api.predictionRoute
        , Mbta.Api.include Mbta.Api.predictionSchedule
        ]
        [ Mbta.Api.filterPredictionsByRouteIds (Selection.selectedRouteIds selections)
        , Mbta.Api.filterPredictionsByStopIds (Selection.selectedStopIds selections)
        ]


streamEventSub : Sub Msg
streamEventSub =
    streamEventPort (\{ eventName, data } -> StreamMsg eventName data)
