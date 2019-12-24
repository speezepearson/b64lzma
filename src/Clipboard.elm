port module Clipboard exposing (..)

import Json.Decode as D
import Json.Encode as E

port userPastedPort : (E.Value -> msg) -> Sub msg

type alias PastedData =
    { html : Maybe String
    , plainText : Maybe String
    }

pastedDataDecoder : D.Decoder PastedData
pastedDataDecoder =
    D.map2 PastedData
        (D.field "html" (D.nullable D.string))
        (D.field "plainText" (D.nullable D.string))

userPasted : (Result D.Error PastedData -> msg) -> Sub msg
userPasted translate =
    userPastedPort
        ( D.decodeValue pastedDataDecoder
        >> Debug.log "got pasted data"
        >> translate
        )
