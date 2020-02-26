module NewSelection exposing (viewModal)

import AssocList as Dict exposing (Dict)
import Color
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import ListHelpers
import Mbta
import Model exposing (..)
import Pill
import ViewHelpers exposing (unit)


viewModal : List Mbta.Route -> NewSelectionState -> Element Msg
viewModal routes newSelectionState =
    case newSelectionState of
        NotMakingNewSelection ->
            El.none

        ChoosingRoute ->
            modalWrapper
                (chooseRoute routes)

        ChoosingStop routeIds directionId loadedStopsByRouteId ->
            modalWrapper
                (chooseStop
                    (selectedRoutes routes routeIds)
                    (ListHelpers.dictKeepOnly routeIds loadedStopsByRouteId)
                )

        ChoosingExtraRoutes routeIds directionId ->
            modalWrapper
                (chooseExtraRoute routes routeIds)


selectedRoutes : List Mbta.Route -> List Mbta.RouteId -> List Mbta.Route
selectedRoutes allRoutes selectedRouteIds =
    List.filter
        (\route ->
            List.member route.id selectedRouteIds
        )
        allRoutes


modalWrapper : Element Msg -> Element Msg
modalWrapper child =
    El.el
        [ El.width El.fill
        , El.height El.fill
        , El.padding (unit * 2)
        , El.behindContent modalBackground

        -- Scrollbars to work around https://github.com/mdgriffith/elm-ui/issues/70
        , El.scrollbars
        ]
        (El.el
            [ Background.color (ViewHelpers.avh4ColorToElmUiColor Color.charcoal)
            , El.centerX
            , El.centerY
            , El.width El.fill
            , El.height El.fill
            , El.scrollbars
            ]
            child
        )


modalBackground : Element Msg
modalBackground =
    El.el
        [ El.width El.fill
        , El.height El.fill
        , Background.color (ViewHelpers.avh4ColorToElmUiColor Color.darkCharcoal)
        , El.alpha 0.5
        , Events.onClick NewSelectionCancel
        ]
        El.none


buttonList : (a -> Msg) -> (a -> Element Msg) -> List a -> Element Msg
buttonList msg viewElem elems =
    El.column
        [ El.width El.fill
        ]
        (List.map
            (\elem ->
                Input.button
                    [ Border.color (ViewHelpers.avh4ColorToElmUiColor Color.lightCharcoal)
                    , Border.widthEach
                        { bottom = 1
                        , left = 0
                        , right = 0
                        , top = 0
                        }
                    , El.width El.fill
                    ]
                    { onPress = Just (msg elem)
                    , label = viewElem elem
                    }
            )
            elems
        )


pillList : List Mbta.Route -> Bool -> Element Msg
pillList routes showAddRouteButton =
    El.wrappedRow
        [ Pill.listSpacing
        ]
        (List.append
            (List.map
                (\route ->
                    Input.button
                        []
                        { onPress = Just (NewSelectionRemoveRoute route.id)
                        , label = Pill.pill route
                        }
                )
                routes
            )
            (if showAddRouteButton then
                [ addPill ]

             else
                []
            )
        )


addPill : Element Msg
addPill =
    Input.button
        [ El.padding 4
        , Border.rounded 24
        , Border.color (ViewHelpers.avh4ColorToElmUiColor Color.white)
        , Border.width 2
        ]
        { onPress = Just NewSelectionAddExtraRoute
        , label =
            El.image
                [ El.width (El.px 16)
                , El.height (El.px 16)
                ]
                { src = "/assets/add.svg"
                , description = "Add route"
                }
        }


header : Bool -> String -> Element Msg
header showBack text =
    El.row
        [ El.width El.fill
        , El.padding (unit // 2)
        , El.spacing (unit // 2)
        ]
        [ El.el
            [ Font.size 24
            ]
            (El.text text)
        , if showBack then
            El.el
                [ El.alignRight
                ]
                (ViewHelpers.iconButton
                    NewSelectionBack
                    { src = "/assets/back.svg"
                    , description = "back"
                    }
                )

          else
            El.none
        , El.el
            [ El.alignRight
            ]
            (ViewHelpers.iconButton
                NewSelectionCancel
                { src = "/assets/close.svg"
                , description = "close"
                }
            )
        ]


chooseRoute : List Mbta.Route -> Element Msg
chooseRoute allRoutes =
    El.column
        [ El.width El.fill
        ]
        [ header False "Choose Route"
        , routeList allRoutes
        ]


chooseExtraRoute : List Mbta.Route -> List Mbta.RouteId -> Element Msg
chooseExtraRoute allRoutes selectedRouteIds =
    El.column
        [ El.width El.fill
        ]
        [ header True "Choose Another Route"
        , pillList (selectedRoutes allRoutes selectedRouteIds) False
        , routeList allRoutes
        ]


routeList : List Mbta.Route -> Element Msg
routeList routes =
    buttonList
        (NewSelectionChoseRoute << .id)
        routeListRoute
        routes


routeListRoute : Mbta.Route -> Element msg
routeListRoute route =
    El.row
        [ El.padding (unit // 4)
        ]
        [ El.el
            [ El.width (El.px (unit * 4))
            ]
            (Pill.pill route)
        , El.paragraph [] [ El.text route.longName ]
        ]


chooseStop : List Mbta.Route -> Dict Mbta.RouteId (List Mbta.Stop) -> Element Msg
chooseStop routes stopsByRouteId =
    let
        stops =
            stopsByRouteId
                |> Dict.values
                |> ListHelpers.listIntersect
    in
    El.column
        [ El.width El.fill
        ]
        [ header True "Choose Stop"
        , pillList routes True
        , stopList stops
        ]


stopList : List Mbta.Stop -> Element Msg
stopList stops =
    buttonList
        (NewSelectionChoseStop << Mbta.stopId)
        stopListStop
        stops


stopListStop : Mbta.Stop -> Element msg
stopListStop stop =
    El.paragraph
        [ El.padding (unit // 2)
        ]
        [ El.text (Mbta.stopName stop) ]
