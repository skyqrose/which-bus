module Modal exposing (view)

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


view : Model -> Element Msg
view model =
    case model.modal of
        NoModal ->
            El.none

        RoutePicker _ ->
            modalWrapper
                (routePicker model.routes)


modalWrapper : Element Msg -> Element Msg
modalWrapper child =
    El.el
        [ El.width El.fill
        , El.height El.fill
        , El.padding (unit * 2)
        , El.behindContent background

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


background : Element Msg
background =
    El.el
        [ El.width El.fill
        , El.height El.fill
        , Background.color (ViewHelpers.avh4ColorToElmUiColor Color.darkCharcoal)
        , El.alpha 0.5
        , Events.onClick CloseModal
        ]
        El.none


routePicker : List Mbta.Route -> Element Msg
routePicker routes =
    El.column
        [ El.width El.fill
        ]
        (List.map
            (\route ->
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
                    { onPress = Just (PickRoute route.id)
                    , label = routePickerRoute route
                    }
            )
            routes
        )


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
