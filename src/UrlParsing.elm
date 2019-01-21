module UrlParsing exposing (parseStopsFromUrl, setStopsInUrl)

import Model exposing (Stop)
import Url
import Url.Parser
import Url.Parser.Query


parseStopsFromUrl : Url.Url -> List Stop
parseStopsFromUrl url =
    -- To avoid having to deal with path parsing, remove the path.
    { url | path = "/" }
        |> Url.Parser.parse stopsUrlParser
        |> Maybe.withDefault []


stopsUrlParser : Url.Parser.Parser (List Stop -> a) a
stopsUrlParser =
    Url.Parser.query stopsQueryParser


stopsQueryParser : Url.Parser.Query.Parser (List Stop)
stopsQueryParser =
    Url.Parser.Query.custom "stop" (List.filterMap parseStop)


parseStop : String -> Maybe Stop
parseStop queryValue =
    case String.split "," queryValue of
        [ routeId, stopId ] ->
            Just
                { routeId = routeId
                , stopId = stopId
                }

        _ ->
            Nothing


setStopsInUrl : List Stop -> Url.Url -> Url.Url
setStopsInUrl stops url =
    let
        queryParams =
            stops
                |> List.map encodeStopAsQueryParam
                |> String.join "&"
    in
    { url | query = Just queryParams }


encodeStopAsQueryParam : Stop -> String
encodeStopAsQueryParam stop =
    String.concat
        [ "stop="
        , stop.routeId
        , ","
        , stop.stopId
        ]
