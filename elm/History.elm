module History exposing (..)

import Array exposing (Array)


type alias History a =
    { snapshots : Array a
    , currentIndex : Int
    , maxSize : Int
    }


init : Int -> a -> History a
init maxSize firstSnapshot =
    { snapshots = Array.fromList [ firstSnapshot ]
    , currentIndex = 0
    , maxSize = maxSize
    }


push : a -> History a -> History a
push snapshot history =
    let
        sliced =
            Array.slice 0 (history.currentIndex + 1) history.snapshots
    in
    { history | snapshots = Array.push snapshot sliced, currentIndex = history.currentIndex + 1 }


count : History a -> Int
count history =
    Array.length history.snapshots


canUndo : History a -> Bool
canUndo history =
    not (Array.isEmpty history.snapshots) && history.currentIndex > 0


canRedo : History a -> Bool
canRedo history =
    history.currentIndex + 1 < Array.length history.snapshots


undo : History a -> ( Maybe a, History a )
undo history =
    if canUndo history then
        ( Array.get (history.currentIndex - 1) history.snapshots
        , { history | currentIndex = history.currentIndex - 1 }
        )

    else
        ( Nothing, history )


redo : History a -> ( Maybe a, History a )
redo history =
    if canRedo history then
        ( Array.get (history.currentIndex + 1) history.snapshots
        , { history | currentIndex = history.currentIndex + 1 }
        )

    else
        ( Nothing, history )


isLatest : History a -> Bool
isLatest history =
    Array.length history.snapshots == history.currentIndex + 1
