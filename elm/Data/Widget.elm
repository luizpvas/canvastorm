module Data.Widget exposing (Widget)

import Data.CommentId exposing (CommentId)
import Data.WidgetId exposing (WidgetId)


type alias Widget =
    { id : WidgetId
    , backgroundColor : String
    , render : WidgetRender
    , commentsIds : List CommentId
    }


type WidgetRender
    = Text
    | Code
    | Todolist
    | Chamado
    | IncomingRequest
