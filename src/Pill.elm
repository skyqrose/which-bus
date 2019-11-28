module Pill exposing (pill)

import Color
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Mbta
import Mbta.Extra
import ViewHelpers


pill : Mbta.RouteId -> Maybe Mbta.Route -> Element msg
pill (Mbta.RouteId routeIdText) maybeRoute =
    El.el
        [ El.padding 4
        , Border.rounded 16
        , Background.color
            (maybeRoute
                |> Maybe.map .color
                |> Maybe.withDefault Color.black
                |> ViewHelpers.avh4ColorToElmUiColor
            )
        , Font.color
            (maybeRoute
                |> Maybe.map .textColor
                |> Maybe.withDefault Color.white
                |> ViewHelpers.avh4ColorToElmUiColor
            )
        ]
        (El.text
            (maybeRoute
                |> Maybe.andThen Mbta.Extra.routeAbbreviation
                |> Maybe.withDefault routeIdText
            )
        )
