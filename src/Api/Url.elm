module Api.Url exposing (url)


url : String -> List ( String, String ) -> String
url path params =
    let
        base =
            "https://api-v3.mbta.com/"

        apiKey =
            "3a6d67c08111426d8617a30340a9fad3"

        paramsWithKey =
            ( "api_key", apiKey ) :: params
    in
    String.concat
        [ base
        , path
        , "?"
        , paramsWithKey
            |> List.map (\( param, value ) -> param ++ "=" ++ value)
            |> String.join "&"
        ]
