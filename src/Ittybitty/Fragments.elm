module Ittybitty.Fragments exposing
    ( Fragment
    , build
    , unwrap
    , getTitle
    , getEncodedBody
    , parse
    , toString
    , parseUrl
    , addToUrl
    , mapUrl
    )

import Regex
import Url
import B64Lzma exposing (B64Lzma(..))

type alias RawFragment = { title : String, encodedBody : B64Lzma }

type Fragment = Fragment RawFragment
type ParseError = ParseError

build : String -> B64Lzma -> Fragment
build title encodedBody =
    Fragment
        { title = title
        , encodedBody = encodedBody
        }

unwrap : Fragment -> RawFragment
unwrap (Fragment raw) = raw

getTitle : Fragment -> String
getTitle (Fragment {title}) = title

getEncodedBody : Fragment -> B64Lzma
getEncodedBody (Fragment {encodedBody}) = encodedBody

toString : Fragment -> String
toString (Fragment {title, encodedBody}) =
    case encodedBody of
        B64Lzma s ->
            (title |> String.replace " " "_" |> Url.percentEncode)
            ++ "/?"
            ++ s

fragmentStringRegex : Regex.Regex
fragmentStringRegex =
    case Regex.fromString "([^/]*)/\\??(.+)" of
        Just re -> re
        Nothing -> Debug.todo "impossible"

parse : String -> Result ParseError Fragment
parse s =
    case List.head (Regex.find fragmentStringRegex s) of
        Nothing -> Err ParseError
        Just {submatches} ->
            let
                decodeTitle : String -> String
                decodeTitle t =
                    t
                    |> Url.percentDecode
                    |> Maybe.withDefault ""
                    |> String.replace "_" " "

                (title, encodedBody) = case submatches of
                    [Nothing, Just b] -> ("", B64Lzma b)
                    [Just t, Just b] -> (decodeTitle t, B64Lzma b)
                    _ -> Debug.todo "impossible: there must be two submatches"
            in
                Ok (build title encodedBody)

parseUrl : Url.Url -> Maybe (Result ParseError Fragment)
parseUrl url =
    url.fragment |> Maybe.map parse
addToUrl : Maybe Fragment -> Url.Url -> Url.Url
addToUrl fragment url =
    { url | fragment = fragment |> Maybe.map toString }

mapUrl : (Fragment -> Fragment) -> Url.Url -> Url.Url
mapUrl f url =
    case parseUrl url of
        Nothing -> url
        Just (Err _) -> url
        Just (Ok fragment) -> addToUrl (Just (f fragment)) url
