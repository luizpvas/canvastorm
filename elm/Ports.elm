port module Ports exposing (shortcutPressed)


port shortcutPressed : (String -> msg) -> Sub msg
