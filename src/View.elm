module View exposing (view)

import Api
import AssocList as Dict
import Browser
import Data exposing (..)
import Element as El exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Model exposing (..)
import Time


view : Model -> Browser.Document Msg
view model =
    { title = "Which Bus - skyqrose"
    , body =
        [ El.layout [] (ui model) ]
    }


ui : Model -> Element Msg
ui model =
    El.column
        [ El.padding unit
        , El.spacing unit
        ]
        (case model.apiResult of
            Api.Loading ->
                [ El.text "Loading..."
                , addSelectionForm model
                ]

            Api.Failure error ->
                [ El.text "Error"
                , El.text (Debug.toString error)
                ]

            Api.Success apiData ->
                [ El.text "Stops"
                , viewSelections
                    model.currentTime
                    model.selections
                    model.stopNames
                    apiData
                , addSelectionForm model
                ]
        )


viewSelections : Time.Posix -> List Selection -> StopNames -> Api.ApiData -> Element msg
viewSelections currentTime selections stopNames apiData =
    El.column
        [ El.spacing unit
        , El.width El.fill
        ]
        (List.map
            (viewSelection currentTime stopNames apiData)
            selections
        )


viewSelection : Time.Posix -> StopNames -> Api.ApiData -> Selection -> Element msg
viewSelection currentTime stopNames apiData selection =
    let
        predictions =
            Api.predictionsForSelection selection apiData

        (RouteId routeIdText) =
            selection.routeId

        (StopId stopIdText) =
            selection.stopId

        stopName =
            stopNames
                |> Dict.get selection.stopId
                |> Maybe.withDefault stopIdText
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
                (El.text stopName)
            ]
        , El.column
            [ El.alignRight
            ]
            (predictions
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


addSelectionForm : Model -> Element Msg
addSelectionForm model =
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
                    (AddSelection
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
