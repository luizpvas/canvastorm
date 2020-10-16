module Selection exposing
    ( Selection
    , calculateSelection
    , clearActiveSelection
    , init
    , recalculateWidgetsArea
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlEvents
import Json.Decode as Decode
import Rect exposing (Rect)
import Widget exposing (Widget, WidgetId)
import World exposing (World)


{-| The selection is an abstraction that holds the selected widgets to do something: scale, delete,
draw a big rectangle around the whole shape.
-}
type alias Selection =
    { widgetsIds : List WidgetId
    , activeSelectionArea : Maybe Rect
    , selectedWidgetsArea : Maybe Rect
    }


init : Selection
init =
    { widgetsIds = []
    , activeSelectionArea = Nothing
    , selectedWidgetsArea = Nothing
    }


{-| Clears the active selection (the rect the user is drawing with their mouse), but keeps selected widgets.
This means the user has stopped selecting and they're ready to _do_ something with the selected widgets: move them,
delete them, etc.
-}
clearActiveSelection : List Widget -> Selection -> Selection
clearActiveSelection _ selection =
    { selection | activeSelectionArea = Nothing }


{-| When the user selects two or more widgets, we need to draw a big rectangle around
all widgets. That is, a rectangle that goes from the most-top-left widget to the
most-bottom-right widget. This function calculates this rectangle.
-}
calculateSelection : Rect -> List Widget -> Selection -> Selection
calculateSelection selectionRect allWidgets _ =
    let
        selectedWidgets =
            List.filter (\widget -> Rect.intersects selectionRect widget.rect) allWidgets
    in
    { widgetsIds = selectedWidgets |> List.map .id
    , activeSelectionArea = Just selectionRect
    , selectedWidgetsArea =
        Just <|
            { x1 = selectedWidgets |> List.map .rect |> List.map .x1 |> List.minimum |> Maybe.withDefault 0
            , y1 = selectedWidgets |> List.map .rect |> List.map .y1 |> List.minimum |> Maybe.withDefault 0
            , x2 = selectedWidgets |> List.map .rect |> List.map .x2 |> List.maximum |> Maybe.withDefault 0
            , y2 = selectedWidgets |> List.map .rect |> List.map .y2 |> List.maximum |> Maybe.withDefault 0
            }
    }


recalculateWidgetsArea : List Widget -> Selection -> Selection
recalculateWidgetsArea allWidgets selection =
    let
        selectedWidgets =
            List.filter (\widget -> List.member widget.id selection.widgetsIds) allWidgets
    in
    { selection
        | selectedWidgetsArea =
            Just <|
                { x1 = selectedWidgets |> List.map .rect |> List.map .x1 |> List.minimum |> Maybe.withDefault 0
                , y1 = selectedWidgets |> List.map .rect |> List.map .y1 |> List.minimum |> Maybe.withDefault 0
                , x2 = selectedWidgets |> List.map .rect |> List.map .x2 |> List.maximum |> Maybe.withDefault 0
                , y2 = selectedWidgets |> List.map .rect |> List.map .y2 |> List.maximum |> Maybe.withDefault 0
                }
    }


type alias Config msg =
    { world : World
    , selection : Selection
    , onStartMoving : msg
    }


view : Config msg -> Html msg
view config =
    div []
        [ viewActiveSelectionArea config
        , viewSelectedWidgetsArea config
        ]


viewActiveSelectionArea : Config msg -> Html msg
viewActiveSelectionArea { world, selection } =
    case selection.activeSelectionArea of
        Nothing ->
            text ""

        Just rect ->
            let
                screenRect =
                    World.rectScreenFromWorld world rect
            in
            div
                [ class "absolute border border-blue-400"
                , style "background" "rgba(48, 193, 255, 0.3)"
                , style "top" (String.fromFloat (Rect.top screenRect) ++ "px")
                , style "left" (String.fromFloat (Rect.left screenRect) ++ "px")
                , style "width" (String.fromFloat (Rect.width screenRect) ++ "px")
                , style "height" (String.fromFloat (Rect.height screenRect) ++ "px")
                ]
                []


viewSelectedWidgetsArea : Config msg -> Html msg
viewSelectedWidgetsArea { world, selection, onStartMoving } =
    case selection.selectedWidgetsArea of
        Nothing ->
            text ""

        Just rect ->
            let
                screenRect =
                    World.rectScreenFromWorld world rect
            in
            if Rect.isZero rect then
                text ""

            else
                div
                    [ class "absolute border border-blue-400"
                    , style "cursor" "move"
                    , style "top" (String.fromFloat (Rect.top screenRect) ++ "px")
                    , style "left" (String.fromFloat (Rect.left screenRect) ++ "px")
                    , style "width" (String.fromFloat (Rect.width screenRect) ++ "px")
                    , style "height" (String.fromFloat (Rect.height screenRect) ++ "px")
                    , HtmlEvents.preventDefaultStopPropagation "mousedown" (Decode.succeed onStartMoving)
                    ]
                    [ div [ class "absolute w-2 h-2 top-0 left-0 -mt-1 -ml-1 bg-blue-400" ] []
                    , div [ class "absolute w-2 h-2 top-0 right-0 -mt-1 -mr-1 bg-blue-400" ] []
                    , div [ class "absolute w-2 h-2 bottom-0 left-0 -mb-1 -ml-1 bg-blue-400" ] []
                    , div [ class "absolute w-2 h-2 bottom-0 right-0 -mb-1 -mr-1 bg-blue-400" ] []
                    ]


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )
