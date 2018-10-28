module View exposing (view)

import Browser
import Html exposing (Html)
import Model exposing (..)


view : Model -> Browser.Document Msg
view model =
    { title = "MBTA Stop Predictions - skyqrose"
    , body =
        [ Html.text "Hello World" ]
    }
