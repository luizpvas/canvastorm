module Colorpicker exposing (Colorpicker(..), Hex, view)

import Array
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlEvents
import Json.Decode as Decode



-- MODEL


type alias Hex =
    String


type Colorpicker
    = PickingHue
    | PickingBrightness Hex


pallete : List { hex : Hex, name : String, shortcut : String, options : List String }
pallete =
    [ { hex = "#A0AEC0"
      , name = "Gray"
      , shortcut = "a"
      , options =
            [ "#F7FAFC"
            , "#EDF2F7"
            , "#E2E8F0"
            , "#CBD5E0"
            , "#A0AEC0"
            , "#718096"
            , "#4A5568"
            , "#2D3748"
            , "#1A202C"
            ]
      }
    , { hex = "#F56565"
      , name = "Red"
      , shortcut = "r"
      , options =
            [ "#FFF5F5"
            , "#FED7D7"
            , "#FEB2B2"
            , "#FC8181"
            , "#F56565"
            , "#E53E3E"
            , "#C53030"
            , "#9B2C2C"
            , "#742A2A"
            ]
      }
    , { hex = "#ED8936"
      , name = "Orange"
      , shortcut = "o"
      , options =
            [ "#FFFAF0"
            , "#FEEBC8"
            , "#FBD38D"
            , "#F6AD55"
            , "#ED8936"
            , "#DD6B20"
            , "#C05621"
            , "#9C4221"
            , "#7B341E"
            ]
      }
    , { hex = "#ECC94B"
      , name = "Yellow"
      , shortcut = "y"
      , options =
            [ "#FFFFF0"
            , "#FEFCBF"
            , "#FAF089"
            , "#F6E05E"
            , "#ECC94B"
            , "#D69E2E"
            , "#B7791F"
            , "#975A16"
            , "#744210"
            ]
      }
    , { hex = "#48BB78"
      , name = "Green"
      , shortcut = "g"
      , options =
            [ "#F0FFF4"
            , "#C6F6D5"
            , "#9AE6B4"
            , "#68D391"
            , "#48BB78"
            , "#38A169"
            , "#2F855A"
            , "#276749"
            , "#22543D"
            ]
      }
    , { hex = "#38B2AC"
      , name = "Teal"
      , shortcut = "t"
      , options =
            [ "#E6FFFA"
            , "#B2F5EA"
            , "#81E6D9"
            , "#4FD1C5"
            , "#38B2AC"
            , "#319795"
            , "#2C7A7B"
            , "#285E61"
            , "#234E52"
            ]
      }
    , { hex = "#4299E1"
      , name = "Blue"
      , shortcut = "b"
      , options =
            [ "#EBF8FF"
            , "#BEE3F8"
            , "#90CDF4"
            , "#63B3ED"
            , "#4299E1"
            , "#3182CE"
            , "#2B6CB0"
            , "#2C5282"
            , "#2A4365"
            ]
      }
    , { hex = "#667EEA"
      , name = "Indigo"
      , shortcut = "i"
      , options =
            [ "#EBF4FF"
            , "#C3DAFE"
            , "#A3BFFA"
            , "#7F9CF5"
            , "#667EEA"
            , "#5A67D8"
            , "#4C51BF"
            , "#434190"
            , "#3C366B"
            ]
      }
    , { hex = "#9F7AEA"
      , name = "Purple"
      , shortcut = "e"
      , options =
            [ "#FAF5FF"
            , "#E9D8FD"
            , "#D6BCFA"
            , "#B794F4"
            , "#9F7AEA"
            , "#805AD5"
            , "#6B46C1"
            , "#553C9A"
            , "#44337A"
            ]
      }
    , { hex = "#ED64A6"
      , name = "Pink"
      , shortcut = "p"
      , options =
            [ "#FFF5F7"
            , "#FED7E2"
            , "#FBB6CE"
            , "#F687B3"
            , "#ED64A6"
            , "#D53F8C"
            , "#B83280"
            , "#97266D"
            , "#702459"
            ]
      }
    ]



-- View


type alias Config msg =
    { colorpicker : Colorpicker
    , hueSelected : Hex -> msg
    , brightnessSelected : Hex -> msg
    }


view : Config msg -> Html msg
view config =
    case config.colorpicker of
        PickingHue ->
            let
                shortcutDecoder =
                    Decode.field "key" Decode.string
                        |> Decode.andThen
                            (\key ->
                                pallete
                                    |> List.filter (\{ shortcut } -> shortcut == key)
                                    |> List.map (\{ hex } -> Decode.succeed (config.hueSelected hex))
                                    |> List.head
                                    |> Maybe.withDefault (Decode.fail "unkown shortcut")
                            )
            in
            div
                [ id "colorpicker-hue"
                , tabindex 0
                , class "bg-white rounded-full p-2 focus:shadow-outline focus:outline-none"
                , HtmlEvents.preventDefaultStopPropagation "keydown" shortcutDecoder
                ]
                (pallete
                    |> List.map (\{ hex } -> hex)
                    |> List.map (\hex -> square (config.hueSelected hex) hex)
                )

        PickingBrightness selectedHue ->
            let
                shortcutDecoder =
                    Decode.field "key" Decode.string
                        |> Decode.map String.toInt
                        |> Decode.andThen
                            (\maybeIndex ->
                                case maybeIndex of
                                    Nothing ->
                                        Decode.fail "not a number"

                                    Just index ->
                                        pallete
                                            |> List.filter (\{ hex } -> hex == selectedHue)
                                            |> List.map .options
                                            |> List.head
                                            |> Maybe.andThen (Array.fromList >> Array.get (index - 1))
                                            |> Maybe.map (\hex -> Decode.succeed (config.brightnessSelected hex))
                                            |> Maybe.withDefault (Decode.fail "unkown shortcut")
                            )
            in
            div
                [ id "colorpicker-brightness"
                , tabindex 0
                , class "bg-white rounded-full p-2 focus:shadow-outline focus:outline-none"
                , HtmlEvents.preventDefaultStopPropagation "keydown" shortcutDecoder
                ]
                (pallete
                    |> List.filter (\{ hex } -> hex == selectedHue)
                    |> List.map .options
                    |> List.head
                    |> Maybe.withDefault []
                    |> List.map (\hex -> square (config.brightnessSelected hex) hex)
                )


square : msg -> Hex -> Html msg
square toMsg hex =
    button [ class "w-4 h-4 rounded-sm", style "background" hex, onClick toMsg ] []
