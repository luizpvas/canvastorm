module Rect exposing
    ( Rect
    , addMargin
    , height
    , intersects
    , isZero
    , left
    , scale
    , shift
    , top
    , width
    , zero
    )

import Point exposing (Point)


type alias Rect =
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    }



-- INIT


zero : Rect
zero =
    { x1 = 0, y1 = 0, x2 = 0, y2 = 0 }



-- UPDATE


shift : Point -> Rect -> Rect
shift { x, y } rect =
    { x1 = rect.x1 + x, x2 = rect.x2 + x, y1 = rect.y1 + y, y2 = rect.y2 + y }


{-| Adds a margin of `val` around the rect, making it `val` units bigger in all directions.
-}
addMargin : Float -> Rect -> Rect
addMargin val rect =
    let
        normalized =
            normalizeTopLeft rect
    in
    { x1 = normalized.x1 - val
    , x2 = normalized.x2 + val
    , y1 = normalized.y1 - val
    , y2 = normalized.y2 + val
    }


scale : Float -> Rect -> Rect
scale f rect =
    { x1 = rect.x1 * f, x2 = rect.x2 * f, y1 = rect.y1 * f, y2 = rect.y2 * f }



-- HELPERS


isZero : Rect -> Bool
isZero rect =
    rect.x1 == 0 && rect.x2 == 0 && rect.y1 == 0 && rect.y2 == 0


intersects : Rect -> Rect -> Bool
intersects rect1 rect2 =
    let
        nrect1 =
            normalizeTopLeft rect1

        nrect2 =
            normalizeTopLeft rect2
    in
    (max nrect1.x1 nrect2.x1 < min nrect1.x2 nrect2.x2)
        && (max nrect1.y1 nrect2.y1 < min nrect1.y2 nrect2.y2)


normalizeTopLeft : Rect -> Rect
normalizeTopLeft { x1, y1, x2, y2 } =
    let
        ( nx1, nx2 ) =
            if x1 > x2 then
                ( x2, x1 )

            else
                ( x1, x2 )

        ( ny1, ny2 ) =
            if y1 > y2 then
                ( y2, y1 )

            else
                ( y1, y2 )
    in
    { x1 = nx1, y1 = ny1, x2 = nx2, y2 = ny2 }


width : Rect -> Float
width rect =
    abs (rect.x2 - rect.x1)


height : Rect -> Float
height rect =
    abs (rect.y2 - rect.y1)


top : Rect -> Float
top rect =
    if rect.y1 < rect.y2 then
        rect.y1

    else
        rect.y2


left : Rect -> Float
left rect =
    if rect.x1 < rect.x2 then
        rect.x1

    else
        rect.x2
