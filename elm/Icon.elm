module Icon exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


pencil : String -> Svg msg
pencil className =
    svg [ class className, fill "none", strokeLinecap "round", strokeLinejoin "round", strokeWidth "2", viewBox "0 0 24 24", stroke "currentColor" ] [ Svg.path [ d "M15.232 5.232l3.536 3.536m-2.036-5.036a2.5 2.5 0 113.536 3.536L6.5 21.036H3v-3.572L16.732 3.732z" ] [] ]


cursor : String -> Svg msg
cursor className =
    svg [ class className, viewBox "0 0 24 24", fill "none", stroke "currentColor", strokeWidth "2", strokeLinecap "round", strokeLinejoin "round", class "feather feather-mouse-pointer" ] [ Svg.path [ d "M3 3l7.07 16.97 2.51-7.39 7.39-2.51L3 3z" ] [], Svg.path [ d "M13 13l6 6" ] [] ]


undo : String -> Svg msg
undo className =
    svg [ class className, viewBox "0 0 24 24" ] [ Svg.path [ d "M12.5 8c-2.6 0-5 1-6.9 2.6L2 7v9h9l-3.6-3.6A8 8 0 0 1 20 16l2.4-.8a10.5 10.5 0 0 0-10-7.2z" ] [] ]
