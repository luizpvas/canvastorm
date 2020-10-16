module World exposing (World, init, pointScreenFromWorld, pointScreenToWorld, rectScreenFromWorld, updatePan, updateZoom)

import Point exposing (Point)
import Rect exposing (Rect)


type alias World =
    { pan : Point
    , zoom : Float
    }


init : World
init =
    { pan = { x = 0, y = 0 }
    , zoom = 1.0
    }


updatePan : Point -> World -> World
updatePan point world =
    { world | pan = point }


updateZoom : Float -> World -> World
updateZoom zoom world =
    { world | zoom = zoom }


pointScreenToWorld : World -> Point -> Point
pointScreenToWorld world point =
    point
        |> Point.minus world.pan
        |> Point.scale (1 / world.zoom)


pointScreenFromWorld : World -> Point -> Point
pointScreenFromWorld world point =
    point
        |> Point.scale world.zoom
        |> Point.plus world.pan


rectScreenFromWorld : World -> Rect -> Rect
rectScreenFromWorld world rect =
    rect
        |> Rect.scale world.zoom
        |> Rect.shift world.pan
