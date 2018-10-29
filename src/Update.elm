module Update exposing (update)

import Browser.Navigation as Navigation
import Model exposing (..)
import Url exposing (Url)
import UrlParsing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlRequest urlRequest ->
            ( model
            , Cmd.none
            )

        OnUrlChange url ->
            ( { model
                | url = url
                , stops = UrlParsing.parseStopsFromUrl url
              }
            , Cmd.none
            )

        AddStop stop ->
            ( model
            , model.url
                |> addQueryParam stop
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


addQueryParam : Stop -> Url -> Url
addQueryParam stop url =
    let
        previousQuery =
            Maybe.withDefault "" url.query

        joiner =
            case previousQuery of
                "" ->
                    ""

                _ ->
                    "&"

        newQuery =
            String.concat
                [ previousQuery
                , joiner
                , "stop="
                , stop.routeId
                , ","
                , stop.stopId
                ]
    in
    { url | query = Just newQuery }
