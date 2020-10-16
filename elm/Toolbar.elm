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
    }


view : Config msg -> Html msg
view config =
    div [ class "absolute top-0 right-0 mt-2 mr-2" ]
        [ div [ class "flex items-center" ]
            [ div [ class "bg-gray-600 text-white py-2 pl-4 pr-12 rounded-full -mr-10 shadow-md" ]
                [ text "Hello"
                ]
            , div [ class "bg-white rounded-full py-2 px-4 space-x-2 flex items-center shadow-md" ]
                [ viewToolbarButton config Tool.Move (Icon.cursor "w-4")
                , viewToolbarButton config Tool.Pencil (Icon.pencil "w-4")
                , viewToolbarButton config
                    (Tool.Colorpicker config.selectedTool Colorpicker.PickingHue)
                    (div [ class "w-4 h-4 rounded", style "background" config.selectedColor ] [])
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


viewToolbarButton : Config msg -> Tool -> Html msg -> Html msg
viewToolbarButton config tool icon =
    let
        className =
            if config.selectedTool == tool then
                "flex items-center bg-blue-600 text-white rounded-md p-1"

            else
                "flex items-center hover:bg-gray-300 rounded-md p-1"
    in
    button [ class className, onClick (config.onSelect tool) ]
        [ icon
        , span [ class "border text-xs leading-none p-px rounded" ] [ text (Tool.toolToShortcut tool) ]
        ]
