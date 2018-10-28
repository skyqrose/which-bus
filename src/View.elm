module View exposing (view)

import Browser
import Element as El exposing (Element)
import Html exposing (Html)
import Model exposing (..)


view : Model -> Browser.Document Msg
view model =
    { title = "MBTA Stop Predictions - skyqrose"
    , body =
        [ El.layout [] ui ]
    }


ui : Element Msg
ui =
    El.text "Hello World"
