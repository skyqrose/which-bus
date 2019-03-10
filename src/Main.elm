port module Main exposing (main)

import Api
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
            Api.init selections
    in
    ( { currentTime = Time.millisToPosix 0
      , url = url
      , navigationKey = key
      , selections = selections
      , apiResult = initApiResult
      , routeIdFormText = ""
      , stopIdFormText = ""
      }
    , Cmd.batch
        [ Task.perform Tick Time.now
        , initApiCmd
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
                    Api.init newSelections
            in
            ( { model
                | url = url
                , selections = newSelections
                , apiResult = initApiResult
              }
            , initApiCmd
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

        ApiMsg apiMsg ->
            ( { model
                | apiResult = Api.update apiMsg model.apiResult
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , Api.subscriptions ApiMsg
        ]
