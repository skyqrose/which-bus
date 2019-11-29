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
import Pill
import Time
import TimeZone
import ViewHelpers
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
        , Background.color (ViewHelpers.avh4ColorToElmUiColor Color.charcoal)
        , Font.color (ViewHelpers.avh4ColorToElmUiColor Color.white)
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
                    (Maybe.withDefault (Time.millisToPosix 0) model.currentTime)
                    model.routesByStopId
                    model.stops
                    data
                    model.selections
                , addSelectionForm model
                , refreshButton model
                ]
        )


viewSelections : Time.Posix -> Dict Mbta.StopId (List Mbta.Route) -> Dict Mbta.StopId Mbta.Stop -> Mbta.Api.Data (List Mbta.Prediction) -> List Selection -> Element Msg
viewSelections currentTime routesByStopId stops data selections =
    El.column
        [ El.spacing (2 * unit)
        , El.width El.fill
        ]
        (List.indexedMap
            (\index selection ->
                let
                    stop : Maybe Mbta.Stop
                    stop =
                        Dict.get selection.stopId stops

                    routes : List Mbta.Route
                    routes =
                        routesByStopId
                            |> Dict.get selection.stopId
                            |> Maybe.withDefault []
                in
                viewSelection
                    index
                    currentTime
                    routes
                    stop
                    data
                    selection
            )
            selections
        )


viewSelection : Int -> Time.Posix -> List Mbta.Route -> Maybe Mbta.Stop -> Mbta.Api.Data (List Mbta.Prediction) -> Selection -> Element Msg
viewSelection index currentTime routes stop data selection =
    let
        ( selectedRoutes, unselectedRoutes ) =
            List.partition
                (\route -> List.member route.id selection.routeIds)
                routes

        unknownSelectedRouteIds : List Mbta.RouteId
        unknownSelectedRouteIds =
            List.filter
                (\routeId ->
                    not
                        (List.member routeId
                            (List.map .id selectedRoutes)
                        )
                )
                selection.routeIds

        hasSelectedRoutes : Bool
        hasSelectedRoutes =
            not (List.isEmpty selectedRoutes) || not (List.isEmpty unknownSelectedRouteIds)
    in
    El.column
        [ El.width El.fill
        , El.spacing unit
        ]
        [ El.row
            [ El.width El.fill
            , El.spacing 4
            ]
            [ selectionStopName stop selection
            , El.el [ El.alignRight ] (directionIcon index selection.directionId)
            , El.el [ El.alignRight ] (removeSelection index)
            ]
        , if hasSelectedRoutes then
            selectedRoutePills index selectedRoutes unknownSelectedRouteIds

          else
            El.none
        , if List.isEmpty unselectedRoutes then
            El.none

          else
            unselectedRoutePills index unselectedRoutes
        , if hasSelectedRoutes then
            viewPredictions currentTime data selection

          else
            El.el
                [ El.padding unit
                ]
                (El.text "Choose routes")
        ]


selectionStopName : Maybe Mbta.Stop -> Selection -> Element Msg
selectionStopName stop selection =
    let
        (Mbta.StopId stopIdText) =
            selection.stopId

        stopName =
            stop
                |> Maybe.map Mbta.stopName
                |> Maybe.withDefault stopIdText
    in
    El.el
        [ Font.size 24
        ]
        (El.text stopName)


directionIcon : Int -> Maybe Mbta.DirectionId -> Element Msg
directionIcon index directionId =
    let
        icon =
            case directionId of
                Nothing ->
                    { src = "/assets/direction-both.svg"
                    , description = "both directions"
                    }

                Just Mbta.D0 ->
                    { src = "/assets/direction-0.svg"
                    , description = "direction 0"
                    }

                Just Mbta.D1 ->
                    { src = "/assets/direction-1.svg"
                    , description = "direction 1"
                    }
    in
    Input.button
        []
        { onPress = Just (ToggleDirection index)
        , label =
            El.image
                [ El.height (El.px 20) ]
                icon
        }


removeSelection : Int -> Element Msg
removeSelection index =
    Input.button
        []
        { onPress = Just (DeleteSelection index)
        , label =
            El.image
                [ El.height (El.px 20)
                ]
                { src = "/assets/close.svg"
                , description = "remove"
                }
        }


selectedRoutePills : Int -> List Mbta.Route -> List Mbta.RouteId -> Element Msg
selectedRoutePills index selectedRoutes unknownSelectedRouteIds =
    El.wrappedRow
        [ El.spacing (unit // 2)
        ]
        (List.concat
            [ List.map
                (\route ->
                    Input.button
                        []
                        { onPress = Just (RemoveRouteFromSelection index route.id)
                        , label = Pill.pill route
                        }
                )
                selectedRoutes
            , List.map
                (\routeId ->
                    Input.button
                        []
                        { onPress = Just (RemoveRouteFromSelection index routeId)
                        , label = Pill.unknownPill routeId
                        }
                )
                unknownSelectedRouteIds
            ]
        )


unselectedRoutePills : Int -> List Mbta.Route -> Element Msg
unselectedRoutePills index unselectedRoutes =
    El.wrappedRow
        [ El.spacing (unit // 2)
        ]
        (List.map
            (\route ->
                Input.button
                    [ El.alpha 0.6
                    ]
                    { onPress = Just (AddRouteToSelection index route.id)
                    , label = Pill.pill route
                    }
            )
            unselectedRoutes
        )


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
        (El.text "No predicted trips")


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
        , Border.color (ViewHelpers.avh4ColorToElmUiColor Color.charcoal)
        , Background.color (ViewHelpers.avh4ColorToElmUiColor prediction.backgroundColor)
        , Font.color (ViewHelpers.avh4ColorToElmUiColor prediction.textColor)
        , Font.size 14
        ]
        [ El.el
            [ El.width (El.px (unit * 8))
            , El.padding (unit // 2)
            , Font.variant Font.tabularNumbers
            ]
            (viewPredictionTime currentTime prediction)
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
            , El.width El.fill
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
            , case prediction.tripName of
                Nothing ->
                    El.none

                Just tripName ->
                    El.text tripName
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
                    El.paragraph
                        [ Font.variant Font.tabularNumbers
                        ]
                        [ El.text "Sched: "
                        , El.el
                            (if predictedTimeString /= scheduledTimeString then
                                [ Font.strike ]

                             else
                                []
                            )
                            (El.text scheduledTimeString)
                        , if predictedTimeString /= scheduledTimeString then
                            El.text (" " ++ predictedTimeString)

                          else
                            El.none
                        ]
            , case prediction.vehicleLabel of
                Nothing ->
                    El.none

                Just vehicleLabel ->
                    El.text vehicleLabel
            ]
        ]


viewPredictionTime : Time.Posix -> ViewModel.ShownPrediction -> El.Element msg
viewPredictionTime currentTime prediction =
    let
        differenceMillis =
            Time.posixToMillis prediction.time - Time.posixToMillis currentTime

        differenceSecs =
            round (toFloat differenceMillis / 1000 / 5) * 5
    in
    if differenceSecs >= 5 * 60 then
        El.paragraph
            [ Font.alignRight
            ]
            [ El.el
                [ Font.size 36
                ]
                (El.text
                    (String.fromInt (round (toFloat differenceSecs / 60)))
                )
            , El.el
                [ Font.size 29
                ]
                (El.text " min")
            ]

    else
        El.el
            [ Font.size 36
            , El.alignRight
            ]
            (El.text
                (String.concat
                    [ if differenceSecs < 0 then
                        "-"

                      else
                        ""
                    , String.fromInt (abs differenceSecs // 60)
                    , ":"
                    , differenceSecs
                        |> abs
                        |> remainderBy 60
                        |> String.fromInt
                        |> String.padLeft 2 '0'
                    ]
                )
            )


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
            [ Font.color (ViewHelpers.avh4ColorToElmUiColor Color.black)
            ]
            { onChange = TypeRouteId
            , text = model.routeIdFormText
            , placeholder = Nothing
            , label = label "Route Ids (period separated)"
            }
        , Input.text
            [ Font.color (ViewHelpers.avh4ColorToElmUiColor Color.black)
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
                if model.stopIdFormText /= "" then
                    Just
                        (AddSelection
                            { routeIds =
                                model.routeIdFormText
                                    |> String.split "."
                                    |> List.filter ((/=) "")
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
            ("Last updated\n" ++ lastUpdatedText model)
        ]


lastUpdatedText : Model -> String
lastUpdatedText model =
    case ( model.currentTime, model.lastUpdated ) of
        ( Just currentTime, Just lastUpdated ) ->
            let
                timeDifferenceMinutes =
                    (Time.posixToMillis currentTime - Time.posixToMillis lastUpdated)
                        // 1000
                        // 60
            in
            String.fromInt timeDifferenceMinutes ++ " minutes ago"

        _ ->
            "Never"


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
