module Pill exposing
    ( pill
    , unknownPill
    )

import Color
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Mbta
import Mbta.Extra
import ViewHelpers


pill : Mbta.Route -> Element msg
pill route =
    El.el
        [ El.padding 4
        , Border.rounded 16
        , Background.color
            (route.color
                |> ViewHelpers.avh4ColorToElmUiColor
            )
        , Font.color
            (route.textColor
                |> ViewHelpers.avh4ColorToElmUiColor
            )
        ]
        (El.text
            (route
                |> Mbta.Extra.routeAbbreviation
                |> Maybe.withDefault route.longName
            )
        )


unknownPill : Mbta.RouteId -> Element msg
unknownPill (Mbta.RouteId routeIdText) =
    El.el
        [ El.padding 4
        , Border.rounded 16
        , Background.color
            (Color.black
                |> ViewHelpers.avh4ColorToElmUiColor
            )
        , Font.color
            (Color.white
                |> ViewHelpers.avh4ColorToElmUiColor
            )
        ]
        (El.text
            routeIdText
        )
