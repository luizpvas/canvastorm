module Editor exposing
    ( Editor
    , addNewDrawingWidget
    , clearSelection
    , init
    , update
    , updateSelection
    , updateWidget
    , updateWidgets
    , updateWorld
    )

import Colorpicker
import Point exposing (Point)
import Selection exposing (Selection)
import Widget exposing (Widget, WidgetId)
import World exposing (World)


type alias Editor =
    { world : World
    , widgets : List Widget
    , selection : Selection
    }


init : Editor
init =
    { world = World.init
    , widgets = []
    , selection = Selection.init
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
