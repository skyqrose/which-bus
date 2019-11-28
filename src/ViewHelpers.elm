module ViewHelpers exposing (avh4ColorToElmUiColor)

import Color
import Element as El


avh4ColorToElmUiColor : Color.Color -> El.Color
avh4ColorToElmUiColor avh4Color =
    let
        { red, green, blue, alpha } =
            Color.toRgba avh4Color
    in
    El.rgba red green blue alpha
