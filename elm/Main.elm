module Main exposing (Model, Msg(..), init, main, update)

import Browser
import Browser.Dom
import Colorpicker exposing (Colorpicker)
import Editor exposing (Editor)
import History exposing (History)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Point exposing (Point)
import Ports
import Rect exposing (Rect)
import Selection
import Task
import Tool exposing (Tool)
import Toolbar
import Widget exposing (Widget, WidgetId)
import World exposing (World)



-- CONSTANTS / CONFIG


zoomSmoothFactor : Float
zoomSmoothFactor =
    250.0


minZoomFactor : Float
minZoomFactor =
    0.3


maxZoomFactor : Float
maxZoomFactor =
    4.0



-- MODEL


type alias Model =
    { latestId : Int
    , editor : Editor
    , history : History Editor
    }


type alias Flags =
    { latestId : Int
    , maxHistorySize : Int
    , embedLeft : Float
    , embedTop : Float
    , embedWidth : Float
    , embedHeight : Float
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        embed =
            Rect.fromLTWH flags.embedLeft flags.embedTop flags.embedWidth flags.embedHeight

        editor =
            Editor.init embed
    in
    ( { latestId = flags.latestId
      , editor = editor
      , history = History.init flags.maxHistorySize editor
      }
    , Task.perform GotViewport Browser.Dom.getViewport
    )



-- UPDATE


type Msg
    = NoOp
    | GotViewport Browser.Dom.Viewport
    | ShortcutPressed String
    | SelectTool Tool
    | StartPanning
    | ApplyPanningDelta Float Float
    | StopPanning
    | StartSelecting Float Float
    | SelectionMove Float Float
    | StopSelecting
    | StartMovingSelection
    | ApplyMovingSelectionDelta Float Float
    | StopMovingSelection
    | ZoomMove Float Float Float
    | StartDrawing Float Float
    | DrawingMove Float Float
    | StopDrawing
    | StartTyping Float Float
    | ColorpickerHueSelected Colorpicker.Hex
    | ColorpickerBrightnessSelected Colorpicker.Hex
    | GotWidgetMsg WidgetId Widget.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotViewport viewport ->
            ( updateEditor (Editor.updateScreen { x = viewport.scene.width, y = viewport.scene.height }) model, Cmd.none )

        ShortcutPressed key ->
            case Tool.toolFromShortcut key model.editor.selectedTool of
                Nothing ->
                    case key of
                        "d" ->
                            ( updateEditor Editor.deleteSelectedWidgets model, Cmd.none )
                                |> commit

                        _ ->
                            ( model, Cmd.none )

                Just tool ->
                    updateSelectedTool tool model

        SelectTool tool ->
            updateSelectedTool tool model

        StartPanning ->
            ( updateEditor (Editor.updateMode Editor.Panning) model, Cmd.none )

        ApplyPanningDelta x y ->
            let
                updatedPan =
                    { x = model.editor.world.pan.x + x, y = model.editor.world.pan.y + y }
            in
            ( updateEditor (Editor.updateWorld (World.updatePan updatedPan)) model, Cmd.none )

        StopPanning ->
            ( updateEditor (Editor.updateMode Editor.Hovering) model, Cmd.none )

        StartSelecting screenX screenY ->
            let
                worldPoint =
                    World.pointScreenToWorld model.editor.world { x = screenX, y = screenY }
            in
            ( model
                |> updateEditor Editor.clearSelection
                |> updateEditor (Editor.updateMode (Editor.Selecting { x1 = worldPoint.x, y1 = worldPoint.y, x2 = worldPoint.x, y2 = worldPoint.y }))
            , Cmd.none
            )

        SelectionMove screenX screenY ->
            case model.editor.mode of
                Editor.Selecting rect ->
                    let
                        worldPoint =
                            World.pointScreenToWorld model.editor.world { x = screenX, y = screenY }

                        updatedRect =
                            { rect | x2 = worldPoint.x, y2 = worldPoint.y }
                    in
                    ( model
                        |> updateEditor (Editor.updateSelection (Selection.calculateSelection updatedRect))
                        |> updateEditor (Editor.updateMode (Editor.Selecting updatedRect))
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        StopSelecting ->
            ( model
                |> updateEditor (Editor.updateSelection Selection.clearActiveSelection)
                |> updateEditor (Editor.updateMode Editor.Hovering)
            , Cmd.none
            )
                |> commit

        StartMovingSelection ->
            ( updateEditor (Editor.updateMode Editor.MovingSelection) model, Cmd.none )

        ApplyMovingSelectionDelta x y ->
            let
                delta =
                    { x = x, y = y }
                        |> Point.scale (1 / model.editor.world.zoom)
            in
            ( model
                |> updateEditor (Editor.updateSelectedWidgets (Widget.updateRect (Rect.shift delta)))
                |> updateEditor (Editor.updateSelection Selection.recalculateWidgetsArea)
            , Cmd.none
            )

        StopMovingSelection ->
            ( updateEditor (Editor.updateMode Editor.Hovering) model, Cmd.none )

        ZoomMove x y delta ->
            let
                smoothedDelta =
                    delta / zoomSmoothFactor

                updatedZoomFactor =
                    model.editor.world.zoom + smoothedDelta

                correctedX =
                    x - model.editor.world.embed.x1 - model.editor.world.pan.x

                correctedY =
                    y - model.editor.world.embed.y1 - model.editor.world.pan.y

                updatedPan =
                    { x = model.editor.world.pan.x - (correctedX * smoothedDelta * 1 / model.editor.world.zoom)
                    , y = model.editor.world.pan.y - (correctedY * smoothedDelta * 1 / model.editor.world.zoom)
                    }

                updatedZoom =
                    Basics.min (Basics.max minZoomFactor updatedZoomFactor) maxZoomFactor
            in
            ( updateEditor (Editor.updateWorld (World.updatePan updatedPan >> World.updateZoom updatedZoom)) model, Cmd.none )

        StartDrawing screenX screenY ->
            let
                ( updatedEditor, nextId ) =
                    Editor.addNewDrawingWidget model.latestId model.editor.selectedColor { x = screenX, y = screenY } model.editor
            in
            ( { model | editor = updatedEditor, latestId = nextId }
                |> updateEditor (Editor.updateMode (Editor.Drawing model.latestId))
            , Cmd.none
            )

        DrawingMove x y ->
            case model.editor.mode of
                Editor.Drawing widgetId ->
                    ( updateEditor (Editor.updateWidget widgetId (Widget.pushWorldPointToDrawing (World.pointScreenToWorld model.editor.world { x = x, y = y }))) model
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        StopDrawing ->
            case model.editor.mode of
                Editor.Drawing widgetId ->
                    ( model
                        |> updateEditor (Editor.updateWidget widgetId Widget.commit)
                        |> updateEditor (Editor.updateMode Editor.Hovering)
                    , Cmd.none
                    )
                        |> commit

                _ ->
                    ( updateEditor (Editor.updateMode Editor.Hovering) model, Cmd.none )

        StartTyping screenX screenY ->
            let
                widgetId =
                    model.latestId

                ( updatedEditor, nextId ) =
                    Editor.addNewTextWidget widgetId { x = screenX, y = screenY } model.editor
            in
            ( { model | editor = updatedEditor, latestId = nextId }
                |> updateEditor (Editor.updateSelection (\_ _ -> Selection.Editing widgetId))
                |> updateEditor (Editor.updateSelectedTool Tool.Move)
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus (Widget.domId widgetId))
            )

        ColorpickerHueSelected hue ->
            case model.editor.selectedTool of
                Tool.Colorpicker previousTool _ ->
                    ( updateEditor (Editor.updateSelectedTool (Tool.Colorpicker previousTool (Colorpicker.PickingBrightness hue))) model
                    , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "colorpicker-brightness")
                    )

                _ ->
                    ( model, Cmd.none )

        ColorpickerBrightnessSelected hex ->
            case model.editor.selectedTool of
                Tool.Colorpicker previousTool _ ->
                    ( model
                        |> updateEditor (Editor.updateSelectedTool previousTool)
                        |> updateEditor (Editor.updateSelectedColor hex)
                    , Cmd.none
                    )
                        |> commit

                _ ->
                    ( model, Cmd.none )

        GotWidgetMsg widgetId subMsg ->
            case Editor.findWidget widgetId model.editor |> Maybe.map (Widget.update subMsg) of
                Nothing ->
                    ( model, Cmd.none )

                Just ( updatedWidget, outgoingMsg ) ->
                    updateEditor (Editor.updateWidget widgetId (\_ -> updatedWidget)) model
                        |> updateOutgoingWidget outgoingMsg


updateOutgoingWidget : Widget.OutgoingMsg -> Model -> ( Model, Cmd Msg )
updateOutgoingWidget msg model =
    case msg of
        Widget.NoOp ->
            ( model, Cmd.none )

        Widget.SelectForEditing widgetId ->
            ( updateEditor (Editor.updateSelection (\_ _ -> Selection.Editing widgetId)) model
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus (Widget.domId widgetId))
            )


updateSelectedTool : Tool -> Model -> ( Model, Cmd Msg )
updateSelectedTool tool model =
    case tool of
        Tool.Move ->
            ( updateEditor (Editor.updateSelectedTool tool) model, Cmd.none )
                |> commit

        Tool.Pencil ->
            ( updateEditor (Editor.updateSelectedTool tool) model, Cmd.none )
                |> commit

        Tool.Text ->
            ( updateEditor (Editor.updateSelectedTool tool) model, Cmd.none )
                |> commit

        Tool.Colorpicker _ _ ->
            ( updateEditor (Editor.updateSelectedTool tool) model
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "colorpicker-hue")
            )

        Tool.Undo ->
            let
                ( maybeEditor, history ) =
                    History.undo model.history
            in
            ( { model | editor = Maybe.withDefault model.editor maybeEditor, history = history }
            , Cmd.none
            )


updateEditor : (Editor -> Editor) -> Model -> Model
updateEditor fn model =
    { model | editor = Editor.update fn model.editor }


commit : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
commit ( model, cmd ) =
    ( { model | history = History.push model.editor model.history }, cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.shortcutPressed ShortcutPressed
        ]



-- GLOBAL EVENTS


globalEvents : Model -> List (Attribute Msg)
globalEvents model =
    case model.editor.mode of
        Editor.Hovering ->
            let
                clickDecoder =
                    Decode.field "which" Decode.int
                        |> Decode.andThen
                            (\code ->
                                case code of
                                    1 ->
                                        case model.editor.selectedTool of
                                            Tool.Move ->
                                                Decode.map2 StartSelecting
                                                    (Decode.field "pageX" Decode.float)
                                                    (Decode.field "pageY" Decode.float)

                                            Tool.Pencil ->
                                                Decode.map2 StartDrawing
                                                    (Decode.field "pageX" Decode.float)
                                                    (Decode.field "pageY" Decode.float)

                                            Tool.Colorpicker _ _ ->
                                                Decode.fail "not needed"

                                            Tool.Text ->
                                                Decode.map2 StartTyping
                                                    (Decode.field "pageX" Decode.float)
                                                    (Decode.field "pageY" Decode.float)

                                            Tool.Undo ->
                                                Decode.fail "not needed"

                                    2 ->
                                        Decode.succeed StartPanning

                                    _ ->
                                        Decode.fail "unknown button"
                            )

                zoomWheelDecoder =
                    Decode.field "ctrlKey" Decode.bool
                        |> Decode.andThen
                            (\ctrlKey ->
                                if ctrlKey then
                                    Decode.map3 ZoomMove
                                        (Decode.field "pageX" Decode.float)
                                        (Decode.field "pageY" Decode.float)
                                        (Decode.field "deltaY" Decode.float |> Decode.map (\v -> v * -1))

                                else
                                    Decode.fail "ctrlKey not pressed"
                            )
            in
            [ preventDefaultOn "mousedown" (Decode.map alwaysPreventDefault clickDecoder)
            , preventDefaultOn "wheel" (Decode.map alwaysPreventDefault zoomWheelDecoder)
            ]

        Editor.Panning ->
            let
                deltaDecoder =
                    Decode.map2 ApplyPanningDelta
                        (Decode.field "movementX" Decode.float)
                        (Decode.field "movementY" Decode.float)
            in
            [ preventDefaultOn "mouseup" (Decode.map alwaysPreventDefault (Decode.succeed StopPanning))
            , preventDefaultOn "mousemove" (Decode.map alwaysPreventDefault deltaDecoder)
            ]

        Editor.Selecting _ ->
            let
                selectionDecoder =
                    Decode.map2 SelectionMove
                        (Decode.field "pageX" Decode.float)
                        (Decode.field "pageY" Decode.float)
            in
            [ preventDefaultOn "mousemove" (Decode.map alwaysPreventDefault selectionDecoder)
            , preventDefaultOn "mouseup" (Decode.map alwaysPreventDefault (Decode.succeed StopSelecting))
            ]

        Editor.Drawing widgetId ->
            let
                drawingDecoder =
                    Decode.map2 DrawingMove
                        (Decode.field "pageX" Decode.float)
                        (Decode.field "pageY" Decode.float)
            in
            [ preventDefaultOn "mousemove" (Decode.map alwaysPreventDefault drawingDecoder)
            , preventDefaultOn "mouseup" (Decode.map alwaysPreventDefault (Decode.succeed StopDrawing))
            ]

        Editor.MovingSelection ->
            let
                deltaDecoder =
                    Decode.map2 ApplyMovingSelectionDelta
                        (Decode.field "movementX" Decode.float)
                        (Decode.field "movementY" Decode.float)
            in
            [ preventDefaultOn "mousemove" (Decode.map alwaysPreventDefault deltaDecoder)
            , preventDefaultOn "mouseup" (Decode.map alwaysPreventDefault (Decode.succeed StopMovingSelection))
            ]


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )



-- VIEW


view : Model -> Html Msg
view model =
    div
        ([ class "relative bg-gray-200 overflow-hidden"
         , viewCursorStyle model
         , style "width" (String.fromFloat (Rect.width model.editor.world.embed) ++ "px")
         , style "height" (String.fromFloat (Rect.height model.editor.world.embed) ++ "px")
         ]
            ++ globalEvents model
        )
        [ viewPage model ]


viewPage : Model -> Html Msg
viewPage model =
    div []
        [ viewBlueprintPattern model.editor
        , viewEditor model
        , Selection.view
            { world = model.editor.world
            , selection = model.editor.selection
            , onStartMoving = StartMovingSelection
            }
        , Toolbar.view
            { selectedTool = model.editor.selectedTool
            , selectedColor = model.editor.selectedColor
            , onSelect = SelectTool
            , hueSelected = ColorpickerHueSelected
            , brightnessSelected = ColorpickerBrightnessSelected
            , undoEnabled = History.canUndo model.history
            }
        ]


viewEditor : Model -> Html Msg
viewEditor model =
    div
        ([ class "relative w-4 h-4 overflow-visible" ]
            ++ viewPanOffsetAndZoomStyle model
        )
        (List.map
            (\widget ->
                Widget.view
                    { widget = widget
                    , isSelected = Editor.isWidgetSelected widget.id model.editor
                    , isEditing = Editor.isEditingWidget widget.id model.editor
                    }
                    |> Html.map (GotWidgetMsg widget.id)
            )
            model.editor.widgets
        )


viewBlueprintPattern : Editor -> Html Msg
viewBlueprintPattern editor =
    let
        backgroundPosition1 =
            String.fromFloat (-2 + editor.world.pan.x)
                ++ "px "
                ++ String.fromFloat (-2 + editor.world.pan.y)
                ++ "px"

        backgroundPosition2 =
            String.fromFloat (-1 + editor.world.pan.x)
                ++ "px "
                ++ String.fromFloat (-1 + editor.world.pan.y)
                ++ "px"

        backgroundPosition =
            style "background-position" (backgroundPosition1 ++ ", " ++ backgroundPosition1 ++ ", " ++ backgroundPosition2 ++ ", " ++ backgroundPosition2)

        backgroundSize1 =
            String.fromFloat (100 * editor.world.zoom)
                ++ "px "
                ++ String.fromFloat (100 * editor.world.zoom)
                ++ "px"

        backgroundSize2 =
            String.fromFloat (20 * editor.world.zoom)
                ++ "px "
                ++ String.fromFloat (20 * editor.world.zoom)
                ++ "px"

        backgroundSize =
            style "background-size" (backgroundSize1 ++ ", " ++ backgroundSize1 ++ ", " ++ backgroundSize2 ++ ", " ++ backgroundSize2)
    in
    div [ class "absolute top-0 left-0 w-screen h-screen blueprint-pattern", backgroundPosition, backgroundSize ] []


viewPanOffsetAndZoomStyle : Model -> List (Attribute msg)
viewPanOffsetAndZoomStyle model =
    let
        translate =
            "translate(" ++ String.fromFloat model.editor.world.pan.x ++ "px, " ++ String.fromFloat model.editor.world.pan.y ++ "px)"

        scale =
            "scale(" ++ String.fromFloat model.editor.world.zoom ++ ")"
    in
    [ style "transform" (translate ++ " " ++ scale), style "transform-origin" "0px 0px" ]


viewCursorStyle : Model -> Attribute msg
viewCursorStyle model =
    case model.editor.selectedTool of
        Tool.Move ->
            case model.editor.mode of
                Editor.Hovering ->
                    style "cursor" "default"

                Editor.Panning ->
                    style "cursor" "grabbing"

                Editor.Selecting _ ->
                    style "cursor" "default"

                Editor.Drawing _ ->
                    style "cursor" "crosshair"

                Editor.MovingSelection ->
                    style "cursor" "move"

        Tool.Pencil ->
            style "cursor" "crosshair"

        Tool.Text ->
            style "cursor" "text"

        Tool.Colorpicker _ _ ->
            style "cursor" "default"

        Tool.Undo ->
            style "cursor" "default"



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
