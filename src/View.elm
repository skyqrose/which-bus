module View exposing (view)

import Browser
import Element as El exposing (Element)
import Element.Input as Input
import Html exposing (Html)
import Model exposing (..)


view : Model -> Browser.Document Msg
view model =
    { title = "MBTA Stop Predictions - skyqrose"
    , body =
        [ El.layout [] (ui model) ]
    }


ui : Model -> Element Msg
ui model =
    El.column []
        [ El.text "Stops"
        , El.column [] (List.map viewStop model.stops)
        , addStopForm model
        ]


viewStop : ( Stop, PredictionsForStop ) -> Element msg
viewStop ( stop, predictions ) =
    El.row []
        [ El.text stop.routeId
        , El.text stop.stopId
        ]


addStopForm : Model -> Element Msg
addStopForm model =
    El.column []
        [ Input.text []
            { onChange = TypeRouteId
            , text = model.routeIdFormText
            , placeholder = Nothing
            , label = label "Route Id"
            }
        , Input.text []
            { onChange = TypeStopId
            , text = model.stopIdFormText
            , placeholder = Nothing
            , label = label "Stop Id"
            }
        , Input.button []
            { onPress =
                Just
                    (AddStop
                        { routeId = model.routeIdFormText
                        , stopId = model.stopIdFormText
                        }
                    )
            , label = El.text "Add Stop"
            }
        ]


label : String -> Input.Label msg
label text =
    Input.labelAbove [] (El.text text)
