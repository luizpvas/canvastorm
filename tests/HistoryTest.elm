module HistoryTest exposing (..)

import Expect exposing (Expectation)
import History exposing (History)
import Test exposing (..)


suite : Test
suite =
    describe "History undo/redo"
        [ test "history manipulation" <|
            \_ ->
                let
                    ( snapshot1, history1 ) =
                        History.init 10 1
                            |> History.push 2
                            |> History.push 3
                            |> History.undo

                    ( snapshot2, history2 ) =
                        history1
                            |> History.push 4
                            |> History.undo

                    ( snapshot3, history3 ) =
                        history2
                            |> History.redo
                in
                Expect.equal [ Just 2, Just 2, Just 4 ] [ snapshot1, snapshot2, snapshot3 ]
        ]
