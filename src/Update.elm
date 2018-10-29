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
            let
                newStops =
                    model.stops ++ [ stop ]
            in
            ( model
            , model.url
                |> setUrlQuery (queryParamsForStops newStops)
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


setUrlQuery : String -> Url -> Url
setUrlQuery query url =
    { url | query = Just query }


queryParamsForStops : List Stop -> String
queryParamsForStops stops =
    stops
        |> List.map
            (\stop ->
                String.concat
                    [ "stop="
                    , stop.routeId
                    , ","
                    , stop.stopId
                    ]
            )
        |> String.join "&"
