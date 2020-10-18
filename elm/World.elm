module World exposing (World, init, pointScreenFromWorld, pointScreenToWorld, rectScreenFromWorld, updatePan, updateZoom)

import Point exposing (Point)
import Rect exposing (Rect)


type alias World =
    { pan : Point
    , zoom : Float
    , embed : Rect
    }


init : Rect -> World
init embed =
    { pan = { x = 0, y = 0 }
    , zoom = 1.0
    , embed = embed
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
        |> Point.minus { x = world.embed.x1, y = world.embed.y1 }
        |> Point.scale (1 / world.zoom)


pointScreenFromWorld : World -> Point -> Point
pointScreenFromWorld world point =
    point
        |> Point.scale world.zoom
        |> Point.plus world.pan
        |> Point.plus { x = world.embed.x1, y = world.embed.y1 }


rectScreenFromWorld : World -> Rect -> Rect
rectScreenFromWorld world rect =
    rect
        |> Rect.scale world.zoom
        |> Rect.shift world.pan
