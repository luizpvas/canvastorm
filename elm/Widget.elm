module Widget exposing
    ( DrawingPointsPosition(..)
    , Msg
    , OutgoingMsg(..)
    , Widget
    , WidgetId
    , WidgetRender(..)
    , commit
    , domId
    , pushWorldPointToDrawing
    , update
    , updateRect
    , view
    )

import Colorpicker
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
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
    | Text String



-- UPDATE


type Msg
    = SetText String
    | SetSize Float Float
    | Select


type OutgoingMsg
    = NoOp
    | SelectForEditing WidgetId


update : Msg -> Widget -> ( Widget, OutgoingMsg )
update msg widget =
    case msg of
        SetText str ->
            case widget.render of
                Drawing _ _ _ ->
                    ( widget, NoOp )

                Text _ ->
                    ( { widget | render = Text str }, NoOp )

        SetSize width height ->
            ( { widget | rect = widget.rect |> Rect.setWidth width |> Rect.setHeight height }, NoOp )

        Select ->
            ( widget, SelectForEditing widget.id )



-- GETTERS


domId : WidgetId -> String
domId widgetId =
    "widget-" ++ String.fromInt widgetId



-- UPDATES


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

        _ ->
            widget


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

        Text _ ->
            widget



-- VIEW


type alias Config =
    { widget : Widget
    , isSelected : Bool
    , isEditing : Bool
    }


view : Config -> Html Msg
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

        Text str ->
            if config.isEditing then
                div
                    [ class "absolute w-10 h-10 bg-red-100"
                    , style "top" (String.fromFloat widget.rect.y1 ++ "px")
                    , style "left" (String.fromFloat widget.rect.x1 ++ "px")
                    ]
                    [ node "canvastorm-widget-textarea"
                        [ id (domId widget.id)
                        , class "border-2 border-red-500"
                        , value str
                        , onInput SetText
                        , on "change-size" sizeDecoder
                        ]
                        []
                    ]

            else
                div
                    [ class "absolute bg-red-100 border-2 whitespace-pre"
                    , style "top" (String.fromFloat widget.rect.y1 ++ "px")
                    , style "left" (String.fromFloat widget.rect.x1 ++ "px")
                    , style "width" (String.fromFloat (Rect.width widget.rect) ++ "px")
                    , style "height" (String.fromFloat (Rect.height widget.rect) ++ "px")
                    , onClick Select
                    ]
                    [ text str ]


sizeDecoder : Decoder Msg
sizeDecoder =
    Decode.map2 SetSize
        (Decode.at [ "detail", "width" ] Decode.float)
        (Decode.at [ "detail", "height" ] Decode.float)
