module Widget exposing
    ( DrawingPointsPosition(..)
    , Widget
    , WidgetId
    , WidgetRender(..)
    , commit
    , pushWorldPointToDrawing
    , update
    , updateRect
    , view
    )

import Colorpicker
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Point exposing (Point)
import Rect exposing (Rect)
import Svg
import Svg.Attributes



-- CONFIG


strokeWidth : Float
strokeWidth =
    3.0


type alias WidgetId =
    Int


type alias Widget =
    { id : WidgetId
    , rect : Rect
    , render : WidgetRender
    }


type DrawingPointsPosition
    = WorldPosition
    | LocalPosition


type WidgetRender
    = Drawing Colorpicker.Hex DrawingPointsPosition (List Point)



-- UPDATES


update : (Widget -> Widget) -> Widget -> Widget
update fn widget =
    fn widget


updateRect : (Rect -> Rect) -> Widget -> Widget
updateRect fn widget =
    { widget | rect = fn widget.rect }


pushWorldPointToDrawing : Point -> Widget -> Widget
pushWorldPointToDrawing { x, y } widget =
    case widget.render of
        Drawing hexColor position points ->
            { widget
                | render = Drawing hexColor position (points ++ [ { x = x, y = y } ])
                , rect =
                    { x1 = Basics.min widget.rect.x1 (x - strokeWidth)
                    , y1 = Basics.min widget.rect.y1 (y - strokeWidth)
                    , x2 = Basics.max widget.rect.x2 (x + strokeWidth)
                    , y2 = Basics.max widget.rect.y2 (y + strokeWidth)
                    }
            }


{-| Called after the userr finishes "drawing" the widget.
-}
commit : Widget -> Widget
commit widget =
    case widget.render of
        Drawing hexColor _ points ->
            let
                shift =
                    \point ->
                        { x = point.x - Rect.left widget.rect
                        , y = point.y - Rect.top widget.rect
                        }
            in
            { widget
                | render = Drawing hexColor LocalPosition (List.map shift points)
            }



-- VIEW


type alias Config =
    { widget : Widget
    , isSelected : Bool
    }


view : Config -> Html msg
view config =
    let
        widget =
            config.widget
    in
    case widget.render of
        Drawing hexColor position points ->
            let
                polylinePoints =
                    case position of
                        WorldPosition ->
                            List.map (\point -> String.fromFloat (point.x - Rect.left widget.rect) ++ "," ++ String.fromFloat (point.y - Rect.top widget.rect)) points
                                |> String.join " "

                        LocalPosition ->
                            List.map (\point -> String.fromFloat point.x ++ "," ++ String.fromFloat point.y) points
                                |> String.join " "

                strokeColor =
                    if config.isSelected then
                        "blue"

                    else
                        hexColor
            in
            Svg.svg
                [ Svg.Attributes.width (String.fromFloat (Rect.width widget.rect) ++ "px")
                , Svg.Attributes.height (String.fromFloat (Rect.height widget.rect) ++ "px")
                , Svg.Attributes.style <| "top: " ++ String.fromFloat widget.rect.y1 ++ "px; left: " ++ String.fromFloat widget.rect.x1 ++ "px;"
                , Svg.Attributes.class "absolute"
                ]
                [ Svg.polyline
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.stroke strokeColor
                    , Svg.Attributes.strokeWidth "3px"
                    , Svg.Attributes.points polylinePoints
                    ]
                    []
                ]
