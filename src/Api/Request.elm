module Api.Request exposing (getStops)

import Api.Decoders
import Api.Types as Api
import Api.Url
import Http
import Json.Decode as Decode


getStops : (Result Http.Error (List Api.Stop) -> msg) -> List Api.StopId -> Cmd msg
getStops msg stopIds =
    let
        stopIdsParam =
            stopIds
                |> List.map (\(Api.StopId stopId) -> stopId)
                |> String.join ","

        url =
            Api.Url.url "stops" [ ( "filter[id]", stopIdsParam ) ]
    in
    Http.get url (Decode.at [ "data" ] (Decode.list Api.Decoders.stopDecoder))
        |> Http.send msg
