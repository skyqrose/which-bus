module View exposing (view)

import Api.Stream
import Api.Types as Api
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
        [ El.layout
            [ Font.family
                [ Font.typeface "Inter"
                , Font.sansSerif
                ]
            ]
            (ui model)
        ]
    }


ui : Model -> Element Msg
ui model =
    El.column
        [ El.padding unit
        , El.spacing unit
        , El.centerX
        , El.width (El.maximum 320 El.fill)
        ]
        (case model.apiResult of
            Api.Stream.Loading ->
                [ El.text "Loading..."
                , addSelectionForm model
                ]

            Api.Stream.Failure error ->
                [ El.text "Error"
                , El.text (Debug.toString error)
                ]

            Api.Stream.Success { lastUpdated, apiData } ->
                [ viewSelections
                    model.currentTime
                    model.selections
                    model.stopNames
                    apiData
                , addSelectionForm model
                , refreshButton model.currentTime lastUpdated
                ]
        )


viewSelections : Time.Posix -> List Selection -> StopNames -> Api.Stream.ApiData -> Element Msg
viewSelections currentTime selections stopNames apiData =
    El.column
        [ El.spacing unit
        , El.width El.fill
        ]
        (List.map
            (viewSelection currentTime stopNames apiData)
            selections
        )


viewSelection : Time.Posix -> StopNames -> Api.Stream.ApiData -> Selection -> Element Msg
viewSelection currentTime stopNames apiData selection =
    El.column
        [ El.width El.fill
        , Border.width 1
        , Border.rounded 4
        ]
        [ selectionHeading stopNames selection
        , viewPredictions currentTime apiData selection
        ]


selectionHeading : StopNames -> Selection -> Element Msg
selectionHeading stopNames selection =
    let
        (Api.RouteId routeIdText) =
            selection.routeId

        (Api.StopId stopIdText) =
            selection.stopId

        directionText =
            case selection.directionId of
                Nothing ->
                    ""

                Just Api.D0 ->
                    " - 0"

                Just Api.D1 ->
                    " - 1"

        stopName =
            stopNames
                |> Dict.get selection.stopId
                |> Maybe.withDefault stopIdText
    in
    El.column
        [ El.padding unit
        , El.width El.fill
        , Border.widthEach
            { bottom = 1
            , left = 0
            , right = 0
            , top = 0
            }
        ]
        [ El.row []
            [ El.text routeIdText
            , El.el [ Font.size fontSmall ] (El.text directionText)
            ]
        , El.el [ Font.size fontSmall ] (El.text stopName)
        ]


viewPredictions : Time.Posix -> Api.Stream.ApiData -> Selection -> Element msg
viewPredictions currentTime apiData selection =
    let
        predictions =
            Api.Stream.predictionsForSelection apiData selection
                |> List.sortBy (.time >> Time.posixToMillis)
                |> List.take 5
    in
    if List.isEmpty predictions then
        El.el
            [ El.padding unit
            ]
            (El.text "---")

    else
        El.table
            [ El.padding unit
            , El.spacingXY unit 0
            ]
            { data = predictions
            , columns =
                [ { header = El.none
                  , width = El.shrink
                  , view =
                        \prediction ->
                            -- Needs two layers of els in order to align right
                            -- el won't align right directly inside a table cell
                            El.el [] <|
                                El.el
                                    [ Font.variant Font.tabularNumbers
                                    , El.alignRight
                                    ]
                                    (El.text (predictionTimeString currentTime prediction))
                  }
                , { header = El.none
                  , width = El.shrink
                  , view =
                        \prediction ->
                            case prediction.platformCode of
                                Nothing ->
                                    El.none

                                Just platformCode ->
                                    El.text platformCode
                  }
                , { header = El.none
                  , width = El.shrink
                  , view =
                        \prediction ->
                            case prediction.vehicleLabel of
                                Nothing ->
                                    El.none

                                Just vehicleLabel ->
                                    El.el
                                        [ Font.size fontSmall ]
                                        (El.text vehicleLabel)
                  }
                , { header = El.none
                  , width = El.fill
                  , view =
                        \prediction ->
                            case prediction.tripHeadsign of
                                Nothing ->
                                    El.none

                                Just headsign ->
                                    El.text headsign
                  }
                ]
            }


predictionTimeString : Time.Posix -> ShownPrediction -> String
predictionTimeString currentTime prediction =
    let
        differenceMillis =
            Time.posixToMillis prediction.time - Time.posixToMillis currentTime

        differenceSecs =
            differenceMillis // 1000

        absSecs =
            abs differenceSecs

        sign =
            if differenceSecs < 0 then
                "-"

            else
                ""

        displayMins =
            String.fromInt (absSecs // 60)

        displaySecs =
            absSecs
                |> remainderBy 60
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    sign ++ displayMins ++ ":" ++ displaySecs


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
        , Input.radioRow []
            { onChange = TypeDirection
            , options =
                [ Input.option Nothing (El.text "None")
                , Input.option (Just Api.D0) (El.text "0")
                , Input.option (Just Api.D1) (El.text "1")
                ]
            , selected = Just model.directionIdFormValue
            , label = label "Direction Id"
            }
        , Input.button
            buttonStyles
            { onPress =
                if model.routeIdFormText /= "" && model.stopIdFormText /= "" then
                    Just
                        (AddSelection
                            { routeId = Api.RouteId model.routeIdFormText
                            , stopId = Api.StopId model.stopIdFormText
                            , directionId = model.directionIdFormValue
                            }
                        )

                else
                    Nothing
            , label = El.text "Add Stop"
            }
        ]


label : String -> Input.Label msg
label text =
    Input.labelAbove [] (El.text text)


refreshButton : Time.Posix -> Time.Posix -> Element Msg
refreshButton currentTime lastUpdated =
    let
        timeDifferenceMinutes =
            (Time.posixToMillis currentTime - Time.posixToMillis lastUpdated)
                // 1000
                // 60
    in
    El.row
        [ El.spacing unit
        , El.width El.fill
        ]
        [ Input.button
            buttonStyles
            { onPress = Just RefreshStream
            , label = El.text "Refresh"
            }
        , El.text
            (String.concat
                [ "Last updated\n"
                , String.fromInt timeDifferenceMinutes
                , " minutes ago"
                ]
            )
        ]


{-| Pixels
-}
unit : Int
unit =
    16


fontSmall : Int
fontSmall =
    14


buttonStyles : List (El.Attribute msg)
buttonStyles =
    [ El.padding (unit // 2)
    , Border.width 1
    , Border.rounded 4
    , Border.shadow
        { offset = ( 1, 1 )
        , size = 0.0
        , blur = 1.0
        , color = El.rgb 0.5 0.5 0.5
        }
    ]
