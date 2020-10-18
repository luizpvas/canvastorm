module Tool exposing (Tool(..), toolFromShortcut, toolToShortcut)

import Colorpicker exposing (Colorpicker)


type Tool
    = Move
    | Pencil
    | Colorpicker Tool Colorpicker
    | Undo


toolFromShortcut : String -> Tool -> Maybe Tool
toolFromShortcut key selectedTool =
    case key of
        "v" ->
            Just Move

        "p" ->
            Just Pencil

        "c" ->
            Just (Colorpicker selectedTool Colorpicker.PickingHue)

        "ctrl+z" ->
            Just Undo

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

        Undo ->
            "Ctrl+Z"
