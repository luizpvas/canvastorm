module Widget.Drawing exposing (DrawingPointsPosition(..), Model, commit, init, pushPoint, view)

import Html exposing (Html)
import Point exposing (Point)
import Rect exposing (Rect)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type DrawingPointsPosition
    = WorldPosition
    | LocalPosition



-- MODEL


type alias Model =
    { color : String
    , position : DrawingPointsPosition
    , points : List Point
    }


init : String -> Point -> Model
init color worldPoint =
    { color = color, position = WorldPosition, points = [ worldPoint ] }


pushPoint : Point -> Model -> Model
pushPoint point model =
    { model | points = model.points ++ [ point ] }


commit : (Point -> Point) -> Model -> Model
commit shift model =
    { model | position = LocalPosition, points = List.map shift model.points }



-- VIEW


type alias Config =
    { model : Model
    , rect : Rect
    , isSelected : Bool
    }


view : Config -> Html msg
view config =
    let
        polylinePoints =
            case config.model.position of
                WorldPosition ->
                    List.map (\point -> String.fromFloat (point.x - Rect.left config.rect) ++ "," ++ String.fromFloat (point.y - Rect.top config.rect)) config.model.points
                        |> String.join " "

                LocalPosition ->
                    List.map (\point -> String.fromFloat point.x ++ "," ++ String.fromFloat point.y) config.model.points
                        |> String.join " "

        strokeColor =
            if config.isSelected then
                "blue"

            else
                config.model.color
    in
    Svg.svg
        [ Svg.Attributes.width (String.fromFloat (Rect.width config.rect) ++ "px")
        , Svg.Attributes.height (String.fromFloat (Rect.height config.rect) ++ "px")
        , Svg.Attributes.style <| "top: " ++ String.fromFloat config.rect.y1 ++ "px; left: " ++ String.fromFloat config.rect.x1 ++ "px;"
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
