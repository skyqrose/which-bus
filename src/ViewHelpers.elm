module ViewHelpers exposing
    ( avh4ColorToElmUiColor
    , iconButton
    , unit
    )

import Color
import Element as El exposing (Element)
import Element.Input as Input


avh4ColorToElmUiColor : Color.Color -> El.Color
avh4ColorToElmUiColor avh4Color =
    let
        { red, green, blue, alpha } =
            Color.toRgba avh4Color
    in
    El.rgba red green blue alpha


{-| Pixels
-}
unit : Int
unit =
    16


iconButton : msg -> { src : String, description : String } -> Element msg
iconButton msg { src, description } =
    Input.button
        []
        { onPress = Just msg
        , label =
            El.image
                [ El.height (El.px 20)
                ]
                { src = src
                , description = description
                }
        }
