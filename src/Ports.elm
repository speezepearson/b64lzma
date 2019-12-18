port module Ports exposing (..)

import Json.Encode as E

port decodeFragment : E.Value -> Cmd msg
port fragmentDecoded : (E.Value -> msg) -> Sub msg
port desiredFragmentSet : (E.Value -> msg) -> Sub msg
