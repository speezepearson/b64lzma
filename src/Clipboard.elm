port module Clipboard exposing (..)

import Json.Decode as D
import Json.Encode as E

port userPastedPort : (E.Value -> msg) -> Sub msg

userPasted : (Result D.Error String -> msg) -> Sub msg
userPasted translate =
    userPastedPort (D.decodeValue D.string >> Debug.log "got pasted data" >> translate)
