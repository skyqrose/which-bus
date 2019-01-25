module UrlParsingTest exposing (suite)

import Expect
import Model exposing (RouteId(..), StopId(..))
import Test exposing (Test, describe, test)
import Url
import Url.Parser
import UrlParsing


suite : Test
suite =
    describe "UrlParsing"
        [ describe "stopsQueryParser"
            [ test "url without query has no stops" <|
                \_ ->
                    Nothing
                        |> urlWithQuery
                        |> UrlParsing.parseStopsFromUrl
                        |> Expect.equal
                            []
            , test "parses one stop" <|
                \_ ->
                    Just "stop=routeId,stopId"
                        |> urlWithQuery
                        |> UrlParsing.parseStopsFromUrl
                        |> Expect.equal
                            [ { routeId = RouteId "routeId"
                              , stopId = StopId "stopId"
                              }
                            ]
            , test "doesn't take a stop with too many ids" <|
                \_ ->
                    Just "stop=one,two,three"
                        |> urlWithQuery
                        |> UrlParsing.parseStopsFromUrl
                        |> Expect.equal
                            []
            , test "doesn't take a stop with too few ids" <|
                \_ ->
                    Just "stop=one"
                        |> urlWithQuery
                        |> UrlParsing.parseStopsFromUrl
                        |> Expect.equal
                            []
            , test "takes multiple stops" <|
                \_ ->
                    Just "stop=routeId1,stopId1&stop=routeId2,stopId2"
                        |> urlWithQuery
                        |> UrlParsing.parseStopsFromUrl
                        |> Expect.equal
                            [ { routeId = RouteId "routeId1"
                              , stopId = StopId "stopId1"
                              }
                            , { routeId = RouteId "routeId2"
                              , stopId = StopId "stopId2"
                              }
                            ]
            , test "includes a stop even if another is badly formatted" <|
                \_ ->
                    Just "stop=one,two,three&stop=routeId,stopId"
                        |> urlWithQuery
                        |> UrlParsing.parseStopsFromUrl
                        |> Expect.equal
                            [ { routeId = RouteId "routeId"
                              , stopId = StopId "stopId"
                              }
                            ]
            , test "includes a stop when there are other query params" <|
                \_ ->
                    Just "other=irrelevant&stop=routeId,stopId"
                        |> urlWithQuery
                        |> UrlParsing.parseStopsFromUrl
                        |> Expect.equal
                            [ { routeId = RouteId "routeId"
                              , stopId = StopId "stopId"
                              }
                            ]
            , test "works when the url has other stuff in it" <|
                \_ ->
                    Just "stop=routeId,stopId"
                        |> fullUrlWithQuery
                        |> UrlParsing.parseStopsFromUrl
                        |> Expect.equal
                            [ { routeId = RouteId "routeId"
                              , stopId = StopId "stopId"
                              }
                            ]
            ]
        ]


urlWithQuery : Maybe String -> Url.Url
urlWithQuery query =
    { protocol = Url.Http
    , host = "www.example.com"
    , port_ = Nothing
    , path = "/"
    , query = query
    , fragment = Nothing
    }


fullUrlWithQuery : Maybe String -> Url.Url
fullUrlWithQuery query =
    { protocol = Url.Http
    , host = "www.example.com"
    , port_ = Nothing
    , path = ""
    , query = query
    , fragment = Just "fragment"
    }


xfullUrlWithQuery : Maybe String -> Url.Url
xfullUrlWithQuery query =
    { protocol = Url.Http
    , host = "www.example.com"
    , port_ = Just 80
    , path = "/path"
    , query = query
    , fragment = Just "#fragment"
    }
