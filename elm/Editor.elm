module Editor exposing
    ( Editor
    , Mode(..)
    , addNewDrawingWidget
    , clearSelection
    , deleteSelectedWidgets
    , init
    , update
    , updateMode
    , updateScreen
    , updateSelectedColor
    , updateSelectedTool
    , updateSelection
    , updateWidget
    , updateWidgets
    , updateWorld
    )

import Colorpicker
import Point exposing (Point)
import Rect exposing (Rect)
import Selection exposing (Selection)
import Tool exposing (Tool)
import Widget exposing (Widget, WidgetId)
import World exposing (World)


type Mode
    = Hovering
    | Panning
    | Selecting Rect
    | Drawing WidgetId
    | MovingSelection


type alias Editor =
    { world : World
    , widgets : List Widget
    , selection : Selection
    , mode : Mode
    , screen : Point
    , selectedTool : Tool
    , selectedColor : String
    }


init : Rect -> Editor
init embed =
    { world = World.init embed
    , widgets = []
    , selection = Selection.init
    , mode = Hovering
    , screen = { x = 0, y = 0 }
    , selectedTool = Tool.Move
    , selectedColor = "#2D3748"
    }


update : (Editor -> Editor) -> Editor -> Editor
update fn editor =
    fn editor


updateWorld : (World -> World) -> Editor -> Editor
updateWorld fn editor =
    { editor | world = fn editor.world }


updateSelection : (List Widget -> Selection -> Selection) -> Editor -> Editor
updateSelection fn editor =
    { editor | selection = fn editor.widgets editor.selection }


updateWidget : WidgetId -> (Widget -> Widget) -> Editor -> Editor
updateWidget id =
    updateWidgets [ id ]


updateScreen : Point -> Editor -> Editor
updateScreen screen editor =
    { editor | screen = screen }


updateMode : Mode -> Editor -> Editor
updateMode mode editor =
    { editor | mode = mode }


updateSelectedTool : Tool -> Editor -> Editor
updateSelectedTool tool editor =
    { editor | selectedTool = tool }


updateSelectedColor : String -> Editor -> Editor
updateSelectedColor hex editor =
    { editor | selectedColor = hex }


updateWidgets : List WidgetId -> (Widget -> Widget) -> Editor -> Editor
updateWidgets ids fn editor =
    let
        updatedWidgets =
            List.map
                (\widget ->
                    if List.member widget.id ids then
                        fn widget

                    else
                        widget
                )
                editor.widgets
    in
    { editor | widgets = updatedWidgets }


deleteSelectedWidgets : Editor -> Editor
deleteSelectedWidgets editor =
    let
        widgets =
            editor.widgets
                |> List.filter (\widget -> not (List.member widget.id editor.selection.widgetsIds))
    in
    { editor | widgets = widgets }
        |> updateSelection (\_ selection -> Selection.init)


clearSelection : Editor -> Editor
clearSelection editor =
    { editor | selection = Selection.init }


addNewDrawingWidget : Int -> Colorpicker.Hex -> Point -> Editor -> ( Editor, Int )
addNewDrawingWidget latestId hexColor screenPoint editor =
    let
        worldPoint =
            World.pointScreenToWorld editor.world screenPoint

        updatedWidgets =
            editor.widgets
                ++ [ { id = latestId
                     , rect = { x1 = worldPoint.x, y1 = worldPoint.y, x2 = worldPoint.x, y2 = worldPoint.y }
                     , render = Widget.Drawing hexColor Widget.WorldPosition [ worldPoint ]
                     }
                   ]
    in
    ( { editor | widgets = updatedWidgets }, latestId + 1 )
