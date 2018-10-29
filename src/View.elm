module View exposing (view)

import Browser
import Element as El exposing (Element)
import Html exposing (Html)
import Model exposing (..)


view : Model -> Browser.Document msg
view model =
    { title = "MBTA Stop Predictions - skyqrose"
    , body =
        [ El.layout [] (ui model) ]
    }


ui : Model -> Element msg
ui model =
    El.column []
        (El.text "Stops"
            :: List.map viewStop model.stops
        )


viewStop : Stop -> Element msg
viewStop stop =
    El.row []
        [ El.text stop.routeId
        , El.text stop.stopId
        ]
