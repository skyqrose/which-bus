module View exposing (view)

import AssocList
import Browser
import Element as El exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Model exposing (..)
import Time


view : Model -> Browser.Document Msg
view model =
    { title = "MBTA Stop Predictions - skyqrose"
    , body =
        [ El.layout [] (ui model) ]
    }


ui : Model -> Element Msg
ui model =
    El.column
        [ El.padding unit
        , El.spacing unit
        ]
        [ El.text "Stops"
        , El.column
            [ El.spacing unit
            , El.width El.fill
            ]
            (viewStops model.currentTime model.stops)
        , addStopForm model
        ]


viewStops : Time.Posix -> StopsData -> List (Element msg)
viewStops currentTime stopsData =
    case stopsData of
        Loading stops ->
            List.map
                (\stop -> viewStop currentTime stop Nothing)
                stops

        Success stopsWithPredictions ->
            stopsWithPredictions
                |> AssocList.map (\stop predictions -> viewStop currentTime stop (Just predictions))
                |> AssocList.values


viewStop : Time.Posix -> Stop -> Maybe PredictionsForStop -> Element msg
viewStop currentTime stop maybePredictions =
    let
        (RouteId routeIdText) =
            stop.routeId

        (StopId stopIdText) =
            stop.stopId
    in
    El.row
        [ El.width El.fill
        , Border.width 1
        , Border.rounded 4
        , El.padding unit
        ]
        [ El.column
            [ El.alignLeft
            ]
            [ El.text routeIdText
            , El.el
                [ Font.size fontSmall
                ]
                (El.text stopIdText)
            ]
        , El.column
            [ El.alignRight
            ]
            (case maybePredictions of
                Nothing ->
                    [ El.text "Loading" ]

                Just predictionsForStop ->
                    predictionsForStop
                        |> AssocList.values
                        |> List.sortBy (.time >> Time.posixToMillis)
                        |> List.take 3
                        |> List.map (predictionTimeString currentTime)
                        |> List.map El.text
            )
        ]


predictionTimeString : Time.Posix -> Prediction -> String
predictionTimeString currentTime prediction =
    let
        differenceMillis =
            Time.posixToMillis prediction.time - Time.posixToMillis currentTime

        differenceSecs =
            differenceMillis // 1000

        displayMins =
            String.fromInt (differenceSecs // 60)

        displaySecs =
            differenceSecs
                |> remainderBy 60
                |> abs
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    displayMins ++ ":" ++ displaySecs


addStopForm : Model -> Element Msg
addStopForm model =
    El.column
        [ El.spacing unit
        ]
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
                        { routeId = RouteId model.routeIdFormText
                        , stopId = StopId model.stopIdFormText
                        }
                    )
            , label = El.text "Add Stop"
            }
        ]


label : String -> Input.Label msg
label text =
    Input.labelAbove [] (El.text text)


{-| Pixels
-}
unit : Int
unit =
    16


fontSmall : Int
fontSmall =
    14
