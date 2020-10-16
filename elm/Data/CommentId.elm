module Data.CommentId exposing (CommentId, decoder, encode, fromInt)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type CommentId
    = CommentId Int


fromInt : Int -> CommentId
fromInt id =
    CommentId id


decoder : Decoder CommentId
decoder =
    Decode.int |> Decode.map CommentId


encode : CommentId -> Value
encode (CommentId id) =
    Encode.int id
