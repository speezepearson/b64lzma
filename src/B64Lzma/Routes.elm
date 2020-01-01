module B64Lzma.Routes exposing
    ( Route
    , indexRoute
    , parseUrl
    , toString
    , setTitle
    )

import Url
import Url.Builder as B
import Url.Parser as P
import Url.Parser.Query as PQ

import B64Lzma.Translation exposing (B64Lzma(..))

type alias Route =
    { title : Maybe String
    , encodedBody : Maybe B64Lzma
    }

indexRoute : Route
indexRoute =
    { title = Nothing
    , encodedBody = Nothing
    }

titleQueryParamName = "title"

flattenMaybe : Maybe (Maybe a) -> Maybe a
flattenMaybe = Maybe.andThen identity

parseUrl : Url.Url -> Route
parseUrl url =
    let
        parser : P.Parser (Maybe String -> a) a
        parser =
            P.query (PQ.string titleQueryParamName)

        title : Maybe String
        title =
            P.parse parser url |> flattenMaybe

        encodedBody : Maybe B64Lzma
        encodedBody =
            Maybe.map B64Lzma url.fragment
    in
        { title = title
        , encodedBody = encodedBody
        }

toString : Route -> String
toString route =
    B.custom
        B.Absolute
        []
        (case route.title of
            Nothing -> []
            Just title -> [B.string titleQueryParamName title])
        (case route.encodedBody of
            Nothing -> Nothing
            Just (B64Lzma encodedBody) -> Just encodedBody)

setTitle : Maybe String -> Route -> Route
setTitle title route =
    { route | title = title }
