module Data.Project exposing (Project, Source(..), decoder, encode, new)

import Data.ProjectId as ProjectId exposing (ProjectId)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Project =
    { id : ProjectId
    , name : String
    , source : Source
    }


type Source
    = Local String



-- HELPERS


new : Int -> ( Project, Int )
new latestId =
    ( { id = ProjectId.fromInt latestId
      , name = ""
      , source = Local ""
      }
    , latestId + 1
    )



-- ENCODE


encode : Project -> Value
encode project =
    Encode.object
        [ ( "id", ProjectId.encode project.id )
        , ( "name", Encode.string project.name )
        , ( "source", encodeSource project.source )
        ]


encodeSource : Source -> Value
encodeSource source =
    case source of
        Local directory ->
            Encode.object
                [ ( "type", Encode.string "local" )
                , ( "directory", Encode.string directory )
                ]



-- DECODER


decoder : Decoder Project
decoder =
    Decode.map3 Project
        (Decode.field "id" ProjectId.decoder)
        (Decode.field "name" Decode.string)
        (Decode.field "source" sourceDecoder)


sourceDecoder : Decoder Source
sourceDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\source ->
                case source of
                    "local" ->
                        Decode.field "directory" Decode.string
                            |> Decode.map Local

                    _ ->
                        Decode.fail <| "Unrecognized project source: " ++ source
            )
