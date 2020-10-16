module HtmlEvents exposing (preventDefaultStopPropagation)

import Html exposing (..)
import Html.Events
import Json.Decode as Decode exposing (Decoder)


preventDefaultStopPropagation : String -> Decoder msg -> Attribute msg
preventDefaultStopPropagation eventName msgDecoder =
    Html.Events.custom eventName (customEventDecoder msgDecoder)


customEventDecoder : Decoder msg -> Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool }
customEventDecoder decoder =
    Decode.map
        (\msg ->
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )
        decoder
