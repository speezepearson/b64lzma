port module Clipboard exposing (..)

import Json.Decode as D
import Json.Encode as E

port userPastedPort : (E.Value -> msg) -> Sub msg

type Error
    = ProtocolError D.Error

type alias PastedData =
    { html : Maybe String
    , plainText : Maybe String
    }

pastedDataDecoder : D.Decoder PastedData
pastedDataDecoder =
    D.map2 PastedData
        (D.field "html" (D.nullable D.string))
        (D.field "plainText" (D.nullable D.string))

userPasted : (Result Error PastedData -> msg) -> Sub msg
userPasted translate =
    userPastedPort
        ( D.decodeValue pastedDataDecoder
        >> Result.mapError ProtocolError
        >> Debug.log "got pasted data"
        >> translate
        )
