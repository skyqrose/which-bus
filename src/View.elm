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
                    model.routes
                    model.stops
                    data
                , addSelectionForm model
                , refreshButton model
                ]
        )


viewSelections : Time.Posix -> List Selection -> Dict Mbta.RouteId Mbta.Route -> Dict Mbta.StopId Mbta.Stop -> Mbta.Api.Data (List Mbta.Prediction) -> Element Msg
viewSelections currentTime selections routes stops data =
    El.column
        [ El.spacing unit
        , El.width El.fill
        ]
        (List.map
            (\selection ->
                let
                    route =
                        Dict.get selection.routeId routes

                    stop =
                        Dict.get selection.stopId stops
                in
                viewSelection currentTime route stop data selection
            )
            selections
        )


viewSelection : Time.Posix -> Maybe Mbta.Route -> Maybe Mbta.Stop -> Mbta.Api.Data (List Mbta.Prediction) -> Selection -> Element Msg
viewSelection currentTime route stop data selection =
    El.column
        [ El.width El.fill
        , Border.width 1
        , Border.rounded 4
        , Background.color
            (route
                |> Maybe.map .color
                |> Maybe.withDefault Color.white
                |> avh4ColorToElmUiColor
            )
        , Font.color
            (route
                |> Maybe.map .textColor
                |> Maybe.withDefault Color.black
                |> avh4ColorToElmUiColor
            )
        ]
        [ selectionHeading route stop selection
        , viewPredictions currentTime data selection
        ]


selectionHeading : Maybe Mbta.Route -> Maybe Mbta.Stop -> Selection -> Element Msg
selectionHeading route stop selection =
    let
        (Mbta.RouteId routeIdText) =
            selection.routeId

        (Mbta.StopId stopIdText) =
            selection.stopId

        directionText =
            case selection.directionId of
                Nothing ->
                    ""

                Just directionId ->
                    route
                        |> Maybe.andThen .directions
                        |> Maybe.map (Mbta.getRouteDirection directionId)
                        |> Maybe.map
                            (\routeDirection ->
                                String.concat
                                    [ routeDirection.name
                                    , " to "
                                    , routeDirection.destination
                                    ]
                            )
                        |> Maybe.withDefault
                            (case directionId of
                                Mbta.D0 ->
                                    "0"

                                Mbta.D1 ->
                                    "1"
                            )
                        |> (\directionName -> " - " ++ directionName)

        routeName =
            case route of
                Nothing ->
                    routeIdText

                Just r ->
                    r.shortName
                        |> Maybe.withDefault r.longName

        stopName =
            stop
                |> Maybe.map Mbta.stopName
                |> Maybe.withDefault stopIdText
    in
    El.column
        [ El.padding unit
        , El.width El.fill
        ]
        [ El.row []
            [ El.text routeName
            , El.el [ Font.size fontSmall ] (El.text directionText)
            ]
        , El.el [ Font.size fontSmall ] (El.text stopName)
        ]


viewPredictions : Time.Posix -> Mbta.Api.Data (List Mbta.Prediction) -> Selection -> Element msg
viewPredictions currentTime data selection =
    let
        predictions =
            ViewModel.predictionsForSelection data selection
                |> List.sortBy (.time >> Time.posixToMillis)
                |> List.take 5
    in
    if List.isEmpty predictions then
        El.el
            [ El.padding unit
            ]
            (El.text "---")

    else
        El.column
            [ El.width El.fill
            ]
            (List.map (viewPrediction currentTime) predictions)


viewPrediction : Time.Posix -> ViewModel.ShownPrediction -> El.Element msg
viewPrediction currentTime prediction =
    El.row
        [ El.width El.fill
        , Border.widthEach
            { bottom = 0
            , left = 0
            , right = 0
            , top = 1
            }
        ]
        [ El.el
            [ El.width (El.px (unit * 8))
            , El.padding (unit // 2)
            , Font.size 36
            , Font.alignRight
            , Font.variant Font.tabularNumbers
            ]
            (El.text (predictionTimeString currentTime prediction))
        , El.column
            [ El.width El.fill
            , El.padding (unit // 2)
            , Border.widthEach
                { bottom = 0
                , left = 1
                , right = 0
                , top = 0
                }
            ]
            [ case prediction.tripHeadsign of
                Nothing ->
                    El.none

                Just headsign ->
                    El.text headsign
            , case prediction.platformCode of
                Nothing ->
                    El.none

                Just platformCode ->
                    El.text platformCode
            , case prediction.vehicleLabel of
                Nothing ->
                    El.none

                Just vehicleLabel ->
                    El.el
                        [ Font.size fontSmall ]
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
                            { routeId = Mbta.RouteId model.routeIdFormText
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
