module Data.Comment exposing (Comment)

import Data.CommentId exposing (CommentId)


type alias Comment =
    { id : CommentId
    , body : String
    }
