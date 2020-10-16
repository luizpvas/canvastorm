module Data.Column exposing (Column, init)

import Data.ColumnId exposing (ColumnId)
import Data.WidgetId exposing (WidgetId)


type alias Column =
    { id : ColumnId
    , title : String
    , widgetsIds : List WidgetId
    }


init : ColumnId -> Column
init id =
    { id = id
    , title = ""
    , widgetsIds = []
    }
