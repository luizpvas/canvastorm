module Widget.Text exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Rect exposing (Rect)


type alias Config msg =
    { text : String
    , rect : Rect
    , domId : String
    , isSelected : Bool
    , isEditing : Bool
    , select : msg
    , setText : String -> msg
    , setSize : Float -> Float -> msg
    }


view : Config msg -> Html msg
view config =
    if config.isEditing then
        div
            [ class "absolute w-10 h-10 bg-red-100"
            , style "top" (String.fromFloat config.rect.y1 ++ "px")
            , style "left" (String.fromFloat config.rect.x1 ++ "px")
            ]
            [ node "canvastorm-widget-textarea"
                [ id config.domId
                , class "border-2 border-red-500"
                , value config.text
                , onInput config.setText
                , on "change-size" (sizeDecoder config.setSize)
                ]
                []
            ]

    else
        div
            [ class "absolute bg-red-100 border-2 whitespace-pre"
            , style "top" (String.fromFloat config.rect.y1 ++ "px")
            , style "left" (String.fromFloat config.rect.x1 ++ "px")
            , style "width" (String.fromFloat (Rect.width config.rect) ++ "px")
            , style "height" (String.fromFloat (Rect.height config.rect) ++ "px")
            , onClick config.select
            ]
            [ text config.text ]


sizeDecoder : (Float -> Float -> msg) -> Decoder msg
sizeDecoder toMsg =
    Decode.map2 toMsg
        (Decode.at [ "detail", "width" ] Decode.float)
        (Decode.at [ "detail", "height" ] Decode.float)
