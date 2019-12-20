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

import Url
import B64Lzma exposing (B64Lzma(..))

type alias RawFragment = { title : String, encodedBody : B64Lzma }

type Fragment = Fragment RawFragment
type ParseError = ParseError

titleBodySeparator = "/?" -- TODO: this should be a regex, `Regex "/\\??"`, to match Ittybitty

build : String -> B64Lzma -> Fragment
build title encodedBody =
    Fragment
        { title = title |> String.replace " " "_" |> Url.percentEncode
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
        B64Lzma s -> title ++ titleBodySeparator ++ s

parse : String -> Result ParseError Fragment
parse s =
    case List.head (String.indexes titleBodySeparator s) of
        Nothing -> Err ParseError
        Just i -> Ok <| Fragment <|
            { title = String.left i s |> Url.percentDecode |> Maybe.withDefault "" |> String.replace "_" " "
            , encodedBody = B64Lzma <| String.dropLeft (i + String.length titleBodySeparator) s
            }

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
