module TextWidgetTest exposing (..)

import Expect exposing (Expectation)
import Main exposing (Model)
import Test exposing (..)
import Tool


suite : Test
suite =
    describe "TextWidget spec"
        [ test "selects the `Move` tool after text is placed" <|
            \_ ->
                let
                    updatedModel =
                        model
                            |> Main.update (Main.SelectTool Tool.Text)
                            |> Tuple.first
                            |> Main.update (Main.StartTyping 0 0)
                            |> Tuple.first
                in
                Expect.equal updatedModel.editor.selectedTool Tool.Move
        ]


model : Model
model =
    Tuple.first <|
        Main.init
            { latestId = 0
            , maxHistorySize = 50
            , embedLeft = 0
            , embedTop = 0
            , embedWidth = 100
            , embedHeight = 100
            }
