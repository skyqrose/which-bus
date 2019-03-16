port module Main exposing (main)

import Api.Stream
import Api.Types as Api
import Api.Url
import AssocList as Dict
import Browser
import Browser.Navigation as Navigation
import Data exposing (..)
import Html
import Http
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
      , directionFormValue = Nothing
      , stopNames = Dict.empty
      , apiResult = initApiResult
      }
    , Cmd.batch
        [ Task.perform Tick Time.now
        , initApiCmd
        , fetchStopNames selections
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
                , fetchStopNames newSelections
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

        TypeDirection direction ->
            ( { model
                | directionFormValue = direction
              }
            , Cmd.none
            )

        ReceiveStopNames result ->
            ( { model
                | stopNames = Result.withDefault Dict.empty result
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


fetchStopNames : List Selection -> Cmd Msg
fetchStopNames selections =
    let
        stopIds =
            selections
                |> List.map .stopId
                |> List.map (\(Api.StopId stopId) -> stopId)
                |> String.join ","

        url =
            Api.Url.url "stops" [ ( "filter[id]", stopIds ) ]
    in
    Http.get url stopNamesDecoder
        |> Http.send ReceiveStopNames


stopNamesDecoder : Decoder StopNames
stopNamesDecoder =
    Decode.map2
        Tuple.pair
        (Decode.at [ "id" ] (Decode.map Api.StopId Decode.string))
        (Decode.at [ "attributes", "name" ] Decode.string)
        |> Decode.list
        |> Decode.at [ "data" ]
        |> Decode.map
            (\namesList ->
                List.foldl
                    (\( id, name ) -> Dict.insert id name)
                    Dict.empty
                    namesList
            )
