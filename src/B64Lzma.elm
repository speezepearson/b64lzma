port module B64Lzma exposing
    ( B64Lzma(..)
    , Error(..)
    , EncodingRelation
    , decode
    , encode
    , decoded
    , encoded
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


type Error
    = ProtocolError D.Error
    | InvalidBase64
    | UnrecognizedError String

errorDecoder : D.Decoder Error
errorDecoder =
    D.map
        (\s -> if s == "invalid base64" then InvalidBase64 else UnrecognizedError s)
        D.string

responseDecoder : D.Decoder (Result Error EncodingRelation)
responseDecoder =
    D.oneOf
        [ D.field "error" (D.map Err errorDecoder)
        , D.map Ok encodingRelationDecoder
        ]

port encodePort : E.Value -> Cmd msg
port decodePort : E.Value -> Cmd msg
port decodedPort : (E.Value -> msg) -> Sub msg
port encodedPort : (E.Value -> msg) -> Sub msg

encode : String -> Cmd msg
encode plaintext =
    plaintext
    |> Debug.log "b64lzma-encoding"
    |> E.string
    |> encodePort
decode : B64Lzma -> Cmd msg
decode (B64Lzma s) =
    s
    |> Debug.log "b64lzma-decoding"
    |> E.string
    |> decodePort

encoded : (Result Error EncodingRelation -> msg) -> Sub msg
encoded translate =
    encodedPort
        ( D.decodeValue responseDecoder
        >> Debug.log "got response"
        >> Result.mapError ProtocolError
        >> Result.andThen identity
        >> translate
        )
decoded : (Result Error EncodingRelation -> msg) -> Sub msg
decoded translate =
    decodedPort
        ( D.decodeValue responseDecoder
        >> Debug.log "got response"
        >> Result.mapError ProtocolError
        >> Result.andThen identity
        >> translate
        )
