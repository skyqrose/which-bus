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
                (routePicker routes)

        ChoosingStop routeIds directionId loadedStops ->
            modalWrapper
                (stopPicker loadedStops)

        ChoosingExtraRoutes routeIds directionId ->
            modalWrapper
                (routePicker routes)


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
            [ El.centerX
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


picker : (a -> Msg) -> (a -> Element Msg) -> List a -> Element Msg
picker msg viewElem elems =
    El.column
        [ El.width El.fill
        ]
        (List.map
            (\elem ->
                Input.button
                    [ Background.color (ViewHelpers.avh4ColorToElmUiColor Color.charcoal)
                    , Border.color (ViewHelpers.avh4ColorToElmUiColor Color.lightCharcoal)
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


routePicker : List Mbta.Route -> Element Msg
routePicker routes =
    picker
        (NewSelectionChoseRoute << .id)
        routePickerRoute
        routes


routePickerRoute : Mbta.Route -> Element msg
routePickerRoute route =
    El.row
        [ El.padding (unit // 4)
        ]
        [ El.el
            [ El.width (El.px (unit * 4))
            ]
            (Pill.pill route)
        , El.paragraph [] [ El.text route.longName ]
        ]


stopPicker : List Mbta.Stop -> Element Msg
stopPicker stops =
    picker
        (NewSelectionChoseStop << Mbta.stopId)
        stopPickerStop
        stops


stopPickerStop : Mbta.Stop -> Element msg
stopPickerStop stop =
    El.text (Mbta.stopName stop)
