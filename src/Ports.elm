port module Ports exposing (decodeFragment, fragmentDecoded)

import Json.Encode as E

port decodeFragment : E.Value -> Cmd msg
port fragmentDecoded : (E.Value -> msg) -> Sub msg
