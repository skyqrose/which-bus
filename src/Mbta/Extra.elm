module Mbta.Extra exposing (routeAbbreviation)

import Mbta


{-| A short abbreviation of a route, maximum 3 letters, suitable for use in icons
-}
routeAbbreviation : Mbta.Route -> Maybe String
routeAbbreviation route =
    case route.shortName of
        Just shortName ->
            Just shortName

        Nothing ->
            case route.id of
                Mbta.RouteId "Red" ->
                    Just "RL"

                Mbta.RouteId "Orange" ->
                    Just "OL"

                Mbta.RouteId "Blue" ->
                    Just "BL"

                Mbta.RouteId "Green-B" ->
                    Just "B"

                Mbta.RouteId "Green-C" ->
                    Just "C"

                Mbta.RouteId "Green-D" ->
                    Just "D"

                Mbta.RouteId "Green-E" ->
                    Just "E"

                Mbta.RouteId "746" ->
                    Just "SLW"

                Mbta.RouteId "CR-Fairmount" ->
                    Just "FMT"

                Mbta.RouteId "CR-Fitchburg" ->
                    Just "FIT"

                Mbta.RouteId "CR-Worcester" ->
                    Just "WOR"

                Mbta.RouteId "CR-Franklin" ->
                    Just "FRK"

                Mbta.RouteId "CR-Greenbush" ->
                    Just "GRN"

                Mbta.RouteId "CR-Haverhill" ->
                    Just "HVL"

                Mbta.RouteId "CR-Kingston" ->
                    Just "KIN"

                Mbta.RouteId "CR-Lowell" ->
                    Just "LWL"

                Mbta.RouteId "CR-Middleborough" ->
                    Just "MID"

                Mbta.RouteId "CR-Needham" ->
                    Just "NDM"

                Mbta.RouteId "CR-Newburyport" ->
                    Just "NEW"

                Mbta.RouteId "CR-Providence" ->
                    Just "PRO"

                Mbta.RouteId "CR-Foxboro" ->
                    Just "FXB"

                _ ->
                    Nothing
