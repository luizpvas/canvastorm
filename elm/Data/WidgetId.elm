module Data.WidgetId exposing (WidgetId, decoder, encode, fromInt)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type WidgetId
    = WidgetId Int


fromInt : Int -> WidgetId
fromInt id =
    WidgetId id


decoder : Decoder WidgetId
decoder =
    Decode.int |> Decode.map WidgetId


encode : WidgetId -> Value
encode (WidgetId id) =
    Encode.int id
