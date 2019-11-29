module NewSelection exposing (viewModal)

import Color
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
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

        ChoosingStop routeIds directionId loadedStops ->
            modalWrapper
                (chooseStop (selectedRoutes routes routeIds) loadedStops)

        ChoosingExtraRoutes routeIds directionId ->
            modalWrapper
                (chooseRoute routes)


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


chooseRoute : List Mbta.Route -> Element Msg
chooseRoute routes =
    El.column
        [ El.width El.fill
        ]
        [ El.el
            [ El.padding (unit // 2)
            , Font.size 24
            ]
            (El.text "Choose Route")
        , routeList routes
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


chooseStop : List Mbta.Route -> List Mbta.Stop -> Element Msg
chooseStop routes stops =
    El.column
        [ El.width El.fill
        ]
        [ El.el
            [ El.padding (unit // 2)
            , Font.size 24
            ]
            (El.text "Choose Stop")
        , El.wrappedRow
            [ Pill.listSpacing
            ]
            (List.map
                Pill.pill
                routes
            )
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
    El.text (Mbta.stopName stop)


addPill : Element Msg
addPill =
    Input.button
        [ El.padding 4
        , Border.rounded 24
        , Border.color (ViewHelpers.avh4ColorToElmUiColor Color.white)
        , Border.width 2
        ]
        { onPress = Just NewSelectionStart
        , label =
            El.image
                [ El.width (El.px 16)
                , El.height (El.px 16)
                ]
                { src = "/assets/add.svg"
                , description = "Add route"
                }
        }
