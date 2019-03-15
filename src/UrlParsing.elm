module UrlParsing exposing (parseSelectionsFromUrl, setSelectionsInUrl)

import Data exposing (..)
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
        [ routeId, stopId ] ->
            Just
                { routeId = RouteId routeId
                , stopId = StopId stopId
                , direction = Nothing
                }

        [ routeId, stopId, "0" ] ->
            Just
                { routeId = RouteId routeId
                , stopId = StopId stopId
                , direction = Just Zero
                }

        [ routeId, stopId, "1" ] ->
            Just
                { routeId = RouteId routeId
                , stopId = StopId stopId
                , direction = Just One
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
        (RouteId routeId) =
            selection.routeId

        (StopId stopId) =
            selection.stopId

        direction =
            case selection.direction of
                Nothing ->
                    ""

                Just Zero ->
                    ",0"

                Just One ->
                    ",1"
    in
    String.concat
        [ "stop="
        , routeId
        , ","
        , stopId
        , direction
        ]
