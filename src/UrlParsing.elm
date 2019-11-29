module UrlParsing exposing (parseSelectionsFromUrl, setSelectionsInUrl)

import Maybe.Extra
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

        directionIdResult : Maybe (Maybe Mbta.DirectionId)
        directionIdResult =
            parseDirectionId directionIdString
    in
    Maybe.andThen
        (\directionId ->
            case parseStopId stopIdString of
                Nothing ->
                    if List.isEmpty routeIds then
                        Nothing

                    else
                        Just
                            (Selection.WithoutStop
                                { routeIds = routeIds
                                , directionId = directionId
                                }
                            )

                Just stopId ->
                    Just
                        (Selection.WithStop
                            { routeIds = routeIds
                            , stopId = stopId
                            , directionId = directionId
                            }
                        )
        )
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
            selection
                |> Selection.routeIds
                |> List.map (\(Mbta.RouteId routeId) -> routeId)
                |> String.join "."

        stopIdString : String
        stopIdString =
            case Selection.stopId selection of
                Just (Mbta.StopId stopId) ->
                    stopId

                Nothing ->
                    ""

        directionIdString : String
        directionIdString =
            case Selection.directionId selection of
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
