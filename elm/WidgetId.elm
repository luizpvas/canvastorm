module WidgetId exposing (WidgetId, decoder, domId, encode, fromInt, next, toInt, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type WidgetId
    = WidgetId Int


next : WidgetId -> WidgetId
next (WidgetId id) =
    WidgetId (id + 1)


domId : WidgetId -> String
domId widgetId =
    "widget-" ++ toString widgetId


fromInt : Int -> WidgetId
fromInt id =
    WidgetId id


toInt : WidgetId -> Int
toInt (WidgetId id) =
    id


toString : WidgetId -> String
toString (WidgetId id) =
    String.fromInt id


encode : WidgetId -> Value
encode (WidgetId id) =
    Encode.int id


decoder : Decoder WidgetId
decoder =
    Decode.map WidgetId Decode.int
