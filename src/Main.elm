port module Main exposing (main)

import Api.Request
import Api.Stream
import Api.Types as Api
import Api.Url
import AssocList as Dict
import Browser
import Browser.Navigation as Navigation
import Data exposing (..)
import Html
import Json.Decode as Decode exposing (Decoder)
import Model exposing (..)
import Task
import Time
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
        selections =
            UrlParsing.parseSelectionsFromUrl url

        ( initApiResult, initApiCmd ) =
            Api.Stream.init selections
    in
    ( { currentTime = Time.millisToPosix 0
      , url = url
      , navigationKey = key
      , selections = selections
      , routeIdFormText = ""
      , stopIdFormText = ""
      , directionIdFormValue = Nothing
      , stopNames = Dict.empty
      , apiResult = initApiResult
      }
    , Cmd.batch
        [ Task.perform Tick Time.now
        , initApiCmd
        , getStopNames selections
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

                ( initApiResult, initApiCmd ) =
                    Api.Stream.init newSelections
            in
            ( { model
                | url = url
                , selections = newSelections
                , apiResult = initApiResult
              }
            , Cmd.batch
                [ initApiCmd
                , getStopNames newSelections
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

        ReceiveStopNames result ->
            ( { model
                | stopNames =
                    result
                        |> Result.withDefault []
                        |> List.map (\stop -> ( stop.id, stop.name ))
                        |> Dict.fromList
              }
            , Cmd.none
            )

        ApiMsg apiMsg ->
            ( { model
                | apiResult = Api.Stream.update apiMsg model.apiResult
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , Api.Stream.subscriptions ApiMsg
        ]


getStopNames : List Selection -> Cmd Msg
getStopNames selections =
    Api.Request.getStops ReceiveStopNames (List.map .stopId selections)
