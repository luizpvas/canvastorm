module Data.Editor exposing (Editor, init)

import Data.Column as Column exposing (Column)
import Data.ColumnId as ColumnId exposing (ColumnId)
import Data.Comment exposing (Comment)
import Data.CommentId exposing (CommentId)
import Data.Widget exposing (Widget)
import Data.WidgetId exposing (WidgetId)
import Dict exposing (Dict)


type alias Editor =
    { columnsIds : List ColumnId
    , columns : Dict Int Column
    , widgets : Dict Int Widget
    , comments : Dict Int Comment
    }


init : Int -> Editor
init latestId =
    { columnsIds = [ ColumnId.fromInt latestId ]
    , columns = Dict.fromList [ ( latestId, Column.init (ColumnId.fromInt latestId) ) ]
    , widgets = Dict.fromList []
    , comments = Dict.fromList []
    }
