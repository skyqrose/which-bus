module UrlParsing exposing (parseSelectionsFromUrl, setSelectionsInUrl)

import Mbta
import Selection exposing (Selection)
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
        [ routeIdsString, stopIdsString ] ->
            parseSelectionFields
                routeIdsString
                stopIdsString
                ""

        [ routeIdsString, stopIdsString, directionIdString ] ->
            parseSelectionFields
                routeIdsString
                stopIdsString
                directionIdString

        _ ->
            Nothing


parseSelectionFields : String -> String -> String -> Maybe Selection
parseSelectionFields routeIdsString stopIdString directionIdString =
    let
        routeIds : List Mbta.RouteId
        routeIds =
            parseRouteIds routeIdsString

        stopIdResult : Maybe Mbta.StopId
        stopIdResult =
            parseStopId stopIdString

        directionIdResult : Maybe (Maybe Mbta.DirectionId)
        directionIdResult =
            parseDirectionId directionIdString
    in
    Maybe.map2
        (Selection routeIds)
        stopIdResult
        directionIdResult


parseRouteIds : String -> List Mbta.RouteId
parseRouteIds routeIdsString =
    routeIdsString
        |> String.split "."
        |> List.filter ((/=) "")
        |> List.map Mbta.RouteId


parseStopId : String -> Maybe Mbta.StopId
parseStopId stopIdString =
    case stopIdString of
        "" ->
            Nothing

        _ ->
            Just (Mbta.StopId stopIdString)


parseDirectionId : String -> Maybe (Maybe Mbta.DirectionId)
parseDirectionId directionIdString =
    case directionIdString of
        "" ->
            Just Nothing

        "0" ->
            Just (Just Mbta.D0)

        "1" ->
            Just (Just Mbta.D1)

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
        routeIdsString : String
        routeIdsString =
            selection.routeIds
                |> List.map (\(Mbta.RouteId routeId) -> routeId)
                |> String.join "."

        (Mbta.StopId stopIdString) =
            selection.stopId

        directionIdString : String
        directionIdString =
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
        , routeIdsString
        , ","
        , stopIdString
        , directionIdString
        ]
