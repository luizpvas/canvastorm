module Editor exposing
    ( Editor
    , Mode(..)
    , addNewDrawingWidget
    , addNewTextWidget
    , clearSelection
    , deleteSelectedWidgets
    , findWidget
    , init
    , isEditingWidget
    , isWidgetSelected
    , update
    , updateMode
    , updateScreen
    , updateSelectedColor
    , updateSelectedTool
    , updateSelectedWidgets
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
import Widget exposing (Widget)
import WidgetId exposing (WidgetId)
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


findWidget : WidgetId -> Editor -> Maybe Widget
findWidget widgetId editor =
    editor.widgets |> List.filter (\widget -> widget.id == widgetId) |> List.head


{-| Is the given widget id currently selected?
-}
isWidgetSelected : WidgetId -> Editor -> Bool
isWidgetSelected widgetId editor =
    List.member widgetId (Selection.widgetsIds editor.selection)


isEditingWidget : WidgetId -> Editor -> Bool
isEditingWidget widgetId editor =
    Selection.isEditing widgetId editor.selection


update : (Editor -> Editor) -> Editor -> Editor
update fn editor =
    fn editor


updateWorld : (World -> World) -> Editor -> Editor
updateWorld fn editor =
    { editor | world = fn editor.world }


updateSelection : (List Widget -> Selection -> Selection) -> Editor -> Editor
updateSelection fn editor =
    { editor | selection = fn editor.widgets editor.selection }


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


updateWidget : WidgetId -> (Widget -> Widget) -> Editor -> Editor
updateWidget id =
    updateWidgets [ id ]


updateSelectedWidgets : (Widget -> Widget) -> Editor -> Editor
updateSelectedWidgets fn editor =
    updateWidgets (Selection.widgetsIds editor.selection) fn editor


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
                |> List.filter (\widget -> not (List.member widget.id (Selection.widgetsIds editor.selection)))
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
                ++ [ { id = WidgetId.fromInt latestId
                     , rect = { x1 = worldPoint.x, y1 = worldPoint.y, x2 = worldPoint.x, y2 = worldPoint.y }
                     , render = Widget.Drawing hexColor Widget.WorldPosition [ worldPoint ]
                     }
                   ]
    in
    ( { editor | widgets = updatedWidgets }, latestId + 1 )


addNewTextWidget : WidgetId -> Point -> Editor -> ( Editor, WidgetId )
addNewTextWidget widgetId anchor editor =
    let
        worldPoint =
            World.pointScreenToWorld editor.world anchor

        updatedWidgets =
            editor.widgets
                ++ [ { id = widgetId
                     , rect = { x1 = worldPoint.x, y1 = worldPoint.y, x2 = worldPoint.x, y2 = worldPoint.y }
                     , render = Widget.Text ""
                     }
                   ]
    in
    ( { editor | widgets = updatedWidgets }, WidgetId.next widgetId )
