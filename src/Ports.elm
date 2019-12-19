port module Ports exposing (..)

import Json.Decode as D
import Json.Encode as E

import Types exposing (Fragment, PageInfo, EncodingRelation)

pageInfoDecoder : D.Decoder PageInfo
pageInfoDecoder =
    D.map2
        PageInfo
        (D.field "title" D.string)
        (D.field "body" D.string)
pageInfoEncoder : PageInfo -> E.Value
pageInfoEncoder info =
    E.object [("title", E.string info.title), ("body", E.string info.body)]

encodingRelationDecoder : D.Decoder EncodingRelation
encodingRelationDecoder =
    D.map2
        EncodingRelation
        (D.field "fragment" D.string)
        (D.field "pageInfo" pageInfoDecoder)

port decodeFragmentPort : E.Value -> Cmd msg
port fragmentDecodedPort : (E.Value -> msg) -> Sub msg

port encodePageInfoPort : E.Value -> Cmd msg
port pageInfoEncodedPort : (E.Value -> msg) -> Sub msg
port userPastedPort : (E.Value -> msg) -> Sub msg

decodeFragment : Fragment -> Cmd msg
decodeFragment fragment =
    decodeFragmentPort <| E.string fragment
fragmentDecoded : (Result D.Error EncodingRelation -> msg) -> Sub msg
fragmentDecoded translate =
    fragmentDecodedPort (Debug.log "got fragmentDecoded" >> D.decodeValue encodingRelationDecoder >> Debug.log "decoded" >> translate >> Debug.log "translated")


encodePageInfo : PageInfo -> Cmd msg
encodePageInfo info =
    encodePageInfoPort <| pageInfoEncoder info
pageInfoEncoded : (Result D.Error EncodingRelation -> msg) -> Sub msg
pageInfoEncoded translate =
    pageInfoEncodedPort (Debug.log "got pageInfoEncoded" >> D.decodeValue encodingRelationDecoder >> Debug.log "decoded" >> translate >> Debug.log "translated")

userPasted : (Result D.Error String -> msg) -> Sub msg
userPasted translate =
    userPastedPort (Debug.log "got userPasted" >> D.decodeValue D.string >> translate)
