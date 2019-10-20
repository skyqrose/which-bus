module UrlParsingTest exposing (suite)

import Data exposing (Selection)
import Expect
import Mbta
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
            , test "parses a selection without a directionId" <|
                \_ ->
                    Just "stop=routeId,stopId"
                        |> urlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            [ { routeIds = [ Mbta.RouteId "routeId" ]
                              , stopId = Mbta.StopId "stopId"
                              , directionId = Nothing
                              }
                            ]
            , test "parses a selection with a directionId" <|
                \_ ->
                    Just "stop=routeId,stopId,1"
                        |> urlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            [ { routeIds = [ Mbta.RouteId "routeId" ]
                              , stopId = Mbta.StopId "stopId"
                              , directionId = Just Mbta.D1
                              }
                            ]
            , test "doesn't take a selection with a bad directionId" <|
                \_ ->
                    Just "stop=one,two,x"
                        |> urlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            []
            , test "parses a selection with multiple route ids" <|
                \_ ->
                    Just "stop=routeId1.routeId2,stopId,1"
                        |> urlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            [ { routeIds = [ Mbta.RouteId "routeId1", Mbta.RouteId "routeId2" ]
                              , stopId = Mbta.StopId "stopId"
                              , directionId = Just Mbta.D1
                              }
                            ]
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
                            [ { routeIds = [ Mbta.RouteId "routeId1" ]
                              , stopId = Mbta.StopId "stopId1"
                              , directionId = Nothing
                              }
                            , { routeIds = [ Mbta.RouteId "routeId2" ]
                              , stopId = Mbta.StopId "stopId2"
                              , directionId = Just Mbta.D0
                              }
                            ]
            , test "includes a selection even if another is badly formatted" <|
                \_ ->
                    Just "stop=one,two,three&stop=routeId,stopId"
                        |> urlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            [ { routeIds = [ Mbta.RouteId "routeId" ]
                              , stopId = Mbta.StopId "stopId"
                              , directionId = Nothing
                              }
                            ]
            , test "includes a selection when there are other query params" <|
                \_ ->
                    Just "other=irrelevant&stop=routeId,stopId"
                        |> urlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            [ { routeIds = [ Mbta.RouteId "routeId" ]
                              , stopId = Mbta.StopId "stopId"
                              , directionId = Nothing
                              }
                            ]
            , test "works when the url has other stuff in it" <|
                \_ ->
                    Just "stop=routeId,stopId"
                        |> fullUrlWithQuery
                        |> UrlParsing.parseSelectionsFromUrl
                        |> Expect.equal
                            [ { routeIds = [ Mbta.RouteId "routeId" ]
                              , stopId = Mbta.StopId "stopId"
                              , directionId = Nothing
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
