module Data.ProjectId exposing (ProjectId, decoder, encode, fromInt)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type ProjectId
    = ProjectId Int


fromInt : Int -> ProjectId
fromInt id =
    ProjectId id


decoder : Decoder ProjectId
decoder =
    Decode.int |> Decode.map ProjectId


encode : ProjectId -> Value
encode (ProjectId id) =
    Encode.int id
