module Tool exposing (Tool(..), toolFromShortcut, toolToShortcut)

import Colorpicker exposing (Colorpicker)


type Tool
    = Move
    | Pencil
    | Colorpicker Tool Colorpicker


toolFromShortcut : String -> Tool -> Maybe Tool
toolFromShortcut key selectedTool =
    case key of
        "v" ->
            Just Move

        "p" ->
            Just Pencil

        "c" ->
            Just (Colorpicker selectedTool Colorpicker.PickingHue)

        _ ->
            Nothing


toolToShortcut : Tool -> String
toolToShortcut tool =
    case tool of
        Move ->
            "V"

        Pencil ->
            "P"

        Colorpicker _ _ ->
            "C"
