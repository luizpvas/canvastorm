module Data.ColumnId exposing (ColumnId, decoder, encode, fromInt)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type ColumnId
    = ColumnId Int


fromInt : Int -> ColumnId
fromInt id =
    ColumnId id


decoder : Decoder ColumnId
decoder =
    Decode.int |> Decode.map ColumnId


encode : ColumnId -> Value
encode (ColumnId id) =
    Encode.int id
