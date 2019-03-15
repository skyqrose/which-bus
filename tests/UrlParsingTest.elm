module UrlParsingTest exposing (suite)

import Data exposing (..)
import Expect
import Test exposing (Test, describe, test)
import Url
import Url.Parser
import UrlParsing


suite : Test
suite =
    describe "UrlParsing"
        [ describe "selectionsQueryParser"
            [ test "url without query has no selections" <|
                \_ ->
                    Nothing
                        |> urlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            []
            , test "parses a selection without a direction" <|
                \_ ->
                    Just "stop=routeId,stopId"
                        |> urlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            [ { routeId = RouteId "routeId"
                              , stopId = StopId "stopId"
                              , direction = Nothing
                              }
                            ]
            , test "parses a selection with a direction" <|
                \_ ->
                    Just "stop=routeId,stopId,1"
                        |> urlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            [ { routeId = RouteId "routeId"
                              , stopId = StopId "stopId"
                              , direction = Just One
                              }
                            ]
            , test "doesn't take a selection with a bad direction id" <|
                \_ ->
                    Just "stop=one,two,x"
                        |> urlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            []
            , test "doesn't take a selection with too many ids" <|
                \_ ->
                    Just "stop=one,two,three,four"
                        |> urlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            []
            , test "doesn't take a selection with too few ids" <|
                \_ ->
                    Just "stop=one"
                        |> urlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            []
            , test "takes multiple selections" <|
                \_ ->
                    Just "stop=routeId1,stopId1&stop=routeId2,stopId2,0"
                        |> urlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            [ { routeId = RouteId "routeId1"
                              , stopId = StopId "stopId1"
                              , direction = Nothing
                              }
                            , { routeId = RouteId "routeId2"
                              , stopId = StopId "stopId2"
                              , direction = Just Zero
                              }
                            ]
            , test "includes a selection even if another is badly formatted" <|
                \_ ->
                    Just "stop=one,two,three&stop=routeId,stopId"
                        |> urlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            [ { routeId = RouteId "routeId"
                              , stopId = StopId "stopId"
                              , direction = Nothing
                              }
                            ]
            , test "includes a selection when there are other query params" <|
                \_ ->
                    Just "other=irrelevant&stop=routeId,stopId"
                        |> urlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            [ { routeId = RouteId "routeId"
                              , stopId = StopId "stopId"
                              , direction = Nothing
                              }
                            ]
            , test "works when the url has other stuff in it" <|
                \_ ->
                    Just "stop=routeId,stopId"
                        |> fullUrlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            [ { routeId = RouteId "routeId"
                              , stopId = StopId "stopId"
                              , direction = Nothing
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
