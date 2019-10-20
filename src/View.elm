module View exposing (view)

import AssocList as Dict exposing (Dict)
import Browser
import Color
import Data exposing (Selection)
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Mbta
import Mbta.Api
import Model exposing (..)
import Time
import TimeZone
import ViewModel


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
        , El.width (El.maximum 400 El.fill)
        , Background.color (avh4ColorToElmUiColor Color.charcoal)
        , Font.color (avh4ColorToElmUiColor Color.white)
        ]
        (case Mbta.Api.streamResult model.streamState of
            Mbta.Api.Loading ->
                [ El.text "Loading..."
                , addSelectionForm model
                , refreshButton model
                ]

            Mbta.Api.Loaded (Err error) ->
                [ El.text "Error"
                , El.text (Debug.toString error)
                , refreshButton model
                ]

            Mbta.Api.Loaded (Ok data) ->
                [ viewSelections
                    model.currentTime
                    model.selections
                    model.stops
                    data
                , addSelectionForm model
                , refreshButton model
                ]
        )


viewSelections : Time.Posix -> List Selection -> Dict Mbta.StopId Mbta.Stop -> Mbta.Api.Data (List Mbta.Prediction) -> Element Msg
viewSelections currentTime selections stops data =
    El.column
        [ El.spacing unit
        , El.width El.fill
        ]
        (List.map
            (\selection ->
                let
                    stop =
                        Dict.get selection.stopId stops
                in
                viewSelection currentTime stop data selection
            )
            selections
        )


viewSelection : Time.Posix -> Maybe Mbta.Stop -> Mbta.Api.Data (List Mbta.Prediction) -> Selection -> Element Msg
viewSelection currentTime stop data selection =
    El.column
        [ El.width El.fill
        ]
        [ selectionHeading stop selection
        , viewPredictions currentTime data selection
        ]


selectionHeading : Maybe Mbta.Stop -> Selection -> Element Msg
selectionHeading stop selection =
    let
        (Mbta.StopId stopIdText) =
            selection.stopId

        stopName =
            stop
                |> Maybe.map Mbta.stopName
                |> Maybe.withDefault stopIdText
    in
    El.el
        [ El.padding unit
        ]
        (El.text stopName)


viewPredictions : Time.Posix -> Mbta.Api.Data (List Mbta.Prediction) -> Selection -> Element msg
viewPredictions currentTime data selection =
    let
        predictions =
            ViewModel.predictionsForSelection data selection
                |> List.sortBy (.time >> Time.posixToMillis)
                |> List.take 5
    in
    El.column
        [ El.width El.fill
        ]
        (if List.isEmpty predictions then
            [ emptyPrediction ]

         else
            List.indexedMap (viewPrediction currentTime) predictions
        )


emptyPrediction : El.Element msg
emptyPrediction =
    El.el
        [ El.padding unit
        ]
        (El.text "---")


viewPrediction : Time.Posix -> Int -> ViewModel.ShownPrediction -> El.Element msg
viewPrediction currentTime index prediction =
    El.row
        [ El.width El.fill
        , Border.widthEach
            { bottom = 0
            , left = 0
            , right = 0
            , top =
                if index == 0 then
                    0

                else
                    4
            }
        , Border.color (avh4ColorToElmUiColor Color.charcoal)
        , Background.color (avh4ColorToElmUiColor prediction.backgroundColor)
        , Font.color (avh4ColorToElmUiColor prediction.textColor)
        , Font.size 14
        ]
        [ El.el
            [ El.width (El.px (unit * 8))
            , El.padding (unit // 2)
            , Font.size 36
            , Font.alignRight
            , Font.variant Font.tabularNumbers
            ]
            (El.text (predictionTimeString currentTime prediction))
        , El.el
            [ El.width (El.px (unit * 2))
            ]
            (case prediction.platformCode of
                Nothing ->
                    El.none

                Just platformCode ->
                    El.el
                        [ El.padding (unit // 4)
                        , El.centerX
                        , Border.width 1
                        , Border.rounded 4
                        ]
                        (El.text platformCode)
            )
        , El.column
            [ El.height El.fill
            , El.padding (unit // 2)
            , El.spacing (unit // 4)
            ]
            [ case prediction.tripHeadsign of
                Nothing ->
                    El.el
                        [ Font.size 16 ]
                        (El.text prediction.routeName)

                Just headsign ->
                    El.row
                        []
                        [ El.el
                            [ Font.size 16 ]
                            (El.text prediction.routeName)
                        , El.text " - "
                        , El.text headsign
                        ]
            , case prediction.scheduledTime of
                Nothing ->
                    El.none

                Just scheduledTime ->
                    let
                        scheduledTimeString =
                            absoluteTimeString scheduledTime

                        predictedTimeString =
                            absoluteTimeString prediction.time
                    in
                    El.row
                        [ El.spacing (unit // 2)
                        ]
                        [ El.text "Sched:"
                        , El.el
                            (if predictedTimeString /= scheduledTimeString then
                                [ Font.strike ]

                             else
                                []
                            )
                            (El.text scheduledTimeString)
                        , if predictedTimeString /= scheduledTimeString then
                            El.text predictedTimeString

                          else
                            El.none
                        ]
            , case prediction.vehicleLabel of
                Nothing ->
                    El.none

                Just vehicleLabel ->
                    El.el
                        []
                        (El.text vehicleLabel)
            ]
        ]


predictionTimeString : Time.Posix -> ViewModel.ShownPrediction -> String
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


absoluteTimeString : Time.Posix -> String
absoluteTimeString time =
    String.concat
        [ time |> Time.toHour timeZone |> String.fromInt |> String.padLeft 2 '0'
        , ":"
        , time |> Time.toMinute timeZone |> String.fromInt |> String.padLeft 2 '0'
        ]


timeZone : Time.Zone
timeZone =
    TimeZone.america__new_york ()


addSelectionForm : Model -> Element Msg
addSelectionForm model =
    El.column
        [ El.spacing unit
        ]
        [ Input.text
            [ Font.color (avh4ColorToElmUiColor Color.black)
            ]
            { onChange = TypeRouteId
            , text = model.routeIdFormText
            , placeholder = Nothing
            , label = label "Route Id"
            }
        , Input.text
            [ Font.color (avh4ColorToElmUiColor Color.black)
            ]
            { onChange = TypeStopId
            , text = model.stopIdFormText
            , placeholder = Nothing
            , label = label "Stop Id"
            }
        , Input.radioRow
            [ El.spacing (unit * 2)
            ]
            { onChange = TypeDirection
            , options =
                [ Input.option Nothing (El.text "None")
                , Input.option (Just Mbta.D0) (El.text "0")
                , Input.option (Just Mbta.D1) (El.text "1")
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
                            { routeIds =
                                model.routeIdFormText
                                    |> String.split "."
                                    |> List.map Mbta.RouteId
                            , stopId = Mbta.StopId model.stopIdFormText
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


refreshButton : Model -> Element Msg
refreshButton model =
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
            ("Last updated\n" ++ lastUpdatedText model.currentTime model.lastUpdated)
        ]


lastUpdatedText : Time.Posix -> Maybe Time.Posix -> String
lastUpdatedText currentTime lastUpdated =
    case lastUpdated of
        Nothing ->
            "Never"

        Just lastUpdatedTime ->
            let
                timeDifferenceMinutes =
                    (Time.posixToMillis currentTime - Time.posixToMillis lastUpdatedTime)
                        // 1000
                        // 60
            in
            String.fromInt timeDifferenceMinutes ++ " minutes ago"


avh4ColorToElmUiColor : Color.Color -> El.Color
avh4ColorToElmUiColor avh4Color =
    let
        { red, green, blue, alpha } =
            Color.toRgba avh4Color
    in
    El.rgba red green blue alpha


{-| Pixels
-}
unit : Int
unit =
    16


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
