module UrlParsing exposing (parseSelectionsFromUrl, setSelectionsInUrl)

import Data exposing (Selection)
import Mbta
import Url
import Url.Parser
import Url.Parser.Query


parseSelectionsFromUrl : Url.Url -> List Selection
parseSelectionsFromUrl url =
    -- To avoid having to deal with path parsing, remove the path.
    { url | path = "/" }
        |> Url.Parser.parse selectionsUrlParser
        |> Maybe.withDefault []


selectionsUrlParser : Url.Parser.Parser (List Selection -> a) a
selectionsUrlParser =
    Url.Parser.query selectionsQueryParser


selectionsQueryParser : Url.Parser.Query.Parser (List Selection)
selectionsQueryParser =
    Url.Parser.Query.custom "stop" (List.filterMap parseSelection)


parseSelection : String -> Maybe Selection
parseSelection queryValue =
    case String.split "," queryValue of
        [ routeIds, stopId ] ->
            Just
                { routeIds =
                    routeIds
                        |> String.split "."
                        |> List.map Mbta.RouteId
                , stopId = Mbta.StopId stopId
                , directionId = Nothing
                }

        [ routeIds, stopId, "0" ] ->
            Just
                { routeIds =
                    routeIds
                        |> String.split "."
                        |> List.map Mbta.RouteId
                , stopId = Mbta.StopId stopId
                , directionId = Just Mbta.D0
                }

        [ routeIds, stopId, "1" ] ->
            Just
                { routeIds =
                    routeIds
                        |> String.split "."
                        |> List.map Mbta.RouteId
                , stopId = Mbta.StopId stopId
                , directionId = Just Mbta.D1
                }

        _ ->
            Nothing


setSelectionsInUrl : List Selection -> Url.Url -> Url.Url
setSelectionsInUrl selections url =
    let
        queryParams =
            selections
                |> List.map encodeSelectionAsQueryParam
                |> String.join "&"
    in
    { url | query = Just queryParams }


encodeSelectionAsQueryParam : Selection -> String
encodeSelectionAsQueryParam selection =
    let
        routeIds =
            String.join "."
                (selection.routeIds
                    |> List.map (\(Mbta.RouteId routeId) -> routeId)
                )

        (Mbta.StopId stopId) =
            selection.stopId

        directionId =
            case selection.directionId of
                Nothing ->
                    ""

                Just Mbta.D0 ->
                    ",0"

                Just Mbta.D1 ->
                    ",1"
    in
    String.concat
        [ "stop="
        , routeIds
        , ","
        , stopId
        , directionId
        ]
