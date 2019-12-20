port module B64Lzma exposing
    ( B64Lzma(..)
    , EncodingRelation
    , b64LzmaEncode
    , b64LzmaDecode
    , b64LzmaResult
    )

import Json.Decode as D
import Json.Encode as E

type B64Lzma = B64Lzma String

type alias EncodingRelation =
    { plaintext : String
    , encoded : B64Lzma
    }

encodingRelationDecoder : D.Decoder EncodingRelation
encodingRelationDecoder =
    D.map2
        EncodingRelation
        (D.field "plaintext" D.string)
        (D.field "encoded" (D.map B64Lzma D.string))


port b64LzmaEncodePort : E.Value -> Cmd msg
port b64LzmaDecodePort : E.Value -> Cmd msg
port b64LzmaResultPort : (E.Value -> msg) -> Sub msg

b64LzmaEncode : String -> Cmd msg
b64LzmaEncode plaintext =
    b64LzmaEncodePort <| E.string plaintext
b64LzmaDecode : B64Lzma -> Cmd msg
b64LzmaDecode (B64Lzma encoded) =
    b64LzmaDecodePort <| E.string encoded

b64LzmaResult : (Result D.Error EncodingRelation -> msg) -> Sub msg
b64LzmaResult translate =
    b64LzmaResultPort (D.decodeValue encodingRelationDecoder >> Debug.log "got relation" >> translate)
