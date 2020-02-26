module ListHelpers exposing
    ( dictKeepOnly
    , listIntersect
    )

import AssocList as Dict exposing (Dict)


listIntersect : List (List a) -> List a
listIntersect lists =
    case lists of
        [] ->
            []

        first :: rest ->
            List.filter
                (\elem -> List.all (List.member elem) rest)
                first


dictKeepOnly : List k -> Dict k v -> Dict k v
dictKeepOnly keys dict =
    Dict.filter
        (\k _ -> List.member k keys)
        dict
