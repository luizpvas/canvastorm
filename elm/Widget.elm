module Widget exposing
    ( Msg
    , OutgoingMsg(..)
    , Widget
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
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Point exposing (Point)
import Rect exposing (Rect)
import Svg
import Svg.Attributes
import Widget.Drawing
import Widget.Text
import WidgetId exposing (WidgetId)



-- CONFIG


strokeWidth : Float
strokeWidth =
    3.0


type alias Widget =
    { id : WidgetId
    , rect : Rect
    , render : WidgetRender
    }


type WidgetRender
    = Drawing Widget.Drawing.Model
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
                Drawing _ ->
                    ( widget, NoOp )

                Text _ ->
                    ( { widget | render = Text str }, NoOp )

        SetSize width height ->
            ( { widget | rect = widget.rect |> Rect.setWidth width |> Rect.setHeight height }, NoOp )

        Select ->
            ( widget, SelectForEditing widget.id )



-- UPDATES


updateRect : (Rect -> Rect) -> Widget -> Widget
updateRect fn widget =
    { widget | rect = fn widget.rect }


pushWorldPointToDrawing : Point -> Widget -> Widget
pushWorldPointToDrawing { x, y } widget =
    case widget.render of
        Drawing model ->
            { widget
                | render = Drawing (Widget.Drawing.pushPoint { x = x, y = y } model)
                , rect =
                    { x1 = Basics.min widget.rect.x1 (x - strokeWidth)
                    , y1 = Basics.min widget.rect.y1 (y - strokeWidth)
                    , x2 = Basics.max widget.rect.x2 (x + strokeWidth)
                    , y2 = Basics.max widget.rect.y2 (y + strokeWidth)
                    }
            }

        _ ->
            widget


{-| Called after the user finishes "drawing" the widget.
-}
commit : Widget -> Widget
commit widget =
    case widget.render of
        Drawing model ->
            let
                shift =
                    \point ->
                        { x = point.x - Rect.left widget.rect
                        , y = point.y - Rect.top widget.rect
                        }
            in
            { widget | render = Drawing (Widget.Drawing.commit shift model) }

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
    case config.widget.render of
        Drawing model ->
            Widget.Drawing.view
                { model = model
                , rect = config.widget.rect
                , isSelected = config.isSelected
                }

        Text str ->
            Widget.Text.view
                { text = str
                , rect = config.widget.rect
                , domId = WidgetId.domId config.widget.id
                , isSelected = config.isSelected
                , isEditing = config.isEditing
                , select = Select
                , setText = SetText
                , setSize = SetSize
                }
