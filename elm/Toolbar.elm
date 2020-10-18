module Toolbar exposing (view, viewToolbarButton)

import Colorpicker exposing (Colorpicker)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon
import Tool exposing (..)


type alias Config msg =
    { selectedTool : Tool
    , selectedColor : Colorpicker.Hex
    , onSelect : Tool -> msg
    , hueSelected : Colorpicker.Hex -> msg
    , brightnessSelected : Colorpicker.Hex -> msg
    , undoEnabled : Bool
    }


view : Config msg -> Html msg
view config =
    div [ class "absolute top-0 right-0 mt-2 mr-2" ]
        [ div [ class "flex items-center" ]
            [ div [ class "bg-white rounded-full py-2 px-4 space-x-2 flex items-center shadow-md" ]
                [ viewToolbarButton config Tool.Move (Icon.cursor "w-4") True
                , viewToolbarButton config Tool.Pencil (Icon.pencil "w-4") True
                , viewToolbarButton config
                    (Tool.Colorpicker config.selectedTool Colorpicker.PickingHue)
                    (div [ class "w-4 h-4 rounded", style "background" config.selectedColor ] [])
                    True
                , viewToolbarButton config Tool.Undo (Icon.undo "w-4") config.undoEnabled
                ]
            ]
        , viewSelectedToolAdvanced config
        ]


viewSelectedToolAdvanced : Config msg -> Html msg
viewSelectedToolAdvanced config =
    case config.selectedTool of
        Tool.Colorpicker previousTool colorpicker ->
            Colorpicker.view
                { colorpicker = colorpicker
                , hueSelected = config.hueSelected
                , brightnessSelected = config.brightnessSelected
                }

        Tool.Move ->
            text ""

        Tool.Pencil ->
            text ""

        Tool.Undo ->
            text ""


viewToolbarButton : Config msg -> Tool -> Html msg -> Bool -> Html msg
viewToolbarButton config tool icon enabled =
    let
        classAttr =
            if config.selectedTool == tool then
                [ class "flex items-center space-x-1 bg-blue-500 text-white rounded-md p-1" ]

            else if enabled then
                [ class "flex items-center space-x-1 hover:bg-gray-300 rounded-md p-1" ]

            else
                [ class "flex items-center space-x-1 rounded-md p-1 opacity-50" ]

        clickAttr =
            if enabled then
                [ onClick (config.onSelect tool) ]

            else
                []
    in
    button (classAttr ++ clickAttr)
        [ icon
        , span [ class "text-xs font-bold" ] [ text (Tool.toolToShortcut tool) ]
        ]
