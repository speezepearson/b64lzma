import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Json.Decode as D
import Json.Encode as E

import B64Lzma exposing (B64Lzma(..))
import Clipboard
import Ittybitty.Fragments as Fragments exposing (Fragment)
import Ittybitty.PageInfo exposing (PageInfo, EncodingRelation)


pushUrl : Nav.Key -> Url.Url -> Cmd msg
pushUrl key url =
    Nav.pushUrl key (Debug.log "pushing url" (Url.toString url))

setFragment : Fragment -> Url.Url -> Url.Url
setFragment fragment url =
    { url | fragment = Just (Fragments.toString fragment) }


-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL


type TranslationState
    = NoFragment
    | InvalidFragment
    | Decoding Fragment
    | Encoding PageInfo
    | Stable (Fragment, PageInfo)

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , translationState : TranslationState
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    startDecoding url {key=key, url=url, translationState=NoFragment}

startDecoding : Url.Url -> Model -> ( Model, Cmd Msg )
startDecoding url model =
    let
        (translationState, cmd) =
            case Fragments.parseUrl url of
                Nothing ->
                    ( NoFragment, Cmd.none )
                Just (Err _) ->
                    ( InvalidFragment, Cmd.none )
                Just (Ok fragment) ->
                    ( Decoding fragment
                    , Fragments.getEncodedBody fragment |> B64Lzma.b64LzmaDecode
                    )
    in
        ( { model | url=url, translationState=translationState }
        , cmd
        )

-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | FragmentDecoded PageInfo
  | PageInfoEncoded Fragment
  | UserPasted String
  | Ignore -- TODO: have better error handling


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, pushUrl model.key url )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
        startDecoding url model

    PageInfoEncoded fragment ->
        ( model
        , pushUrl model.key (Fragments.addToUrl (Just fragment) model.url)
        )

    FragmentDecoded pageInfo ->
        case model.translationState of
            Decoding fragment ->
                ( { model | translationState = Stable (fragment, pageInfo) }
                , Cmd.none
                )
            _ -> Debug.log "got unexpected FragmentDecoded" ( model, Cmd.none )

    UserPasted body ->
        ( { model | translationState = Encoding {title="", body=body} }
        , B64Lzma.b64LzmaEncode body
        )

    Ignore ->
      ( model, Cmd.none )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        interpretB64LzmaResult : B64Lzma.EncodingRelation -> Msg
        interpretB64LzmaResult relation =
            case model.translationState of
                Encoding pageInfo ->
                    if relation.plaintext == pageInfo.body
                        then PageInfoEncoded (Fragments.build pageInfo.title relation.encoded)
                        else Ignore
                Decoding fragment ->
                    let
                        {title, encodedBody} = Fragments.unwrap fragment
                    in
                        if relation.encoded == encodedBody
                            then FragmentDecoded {title=title, body=relation.plaintext}
                            else Ignore
                _ -> Ignore
    in
        Sub.batch
            [ B64Lzma.b64LzmaResult (Result.map interpretB64LzmaResult >> Result.withDefault Ignore)
            , Clipboard.userPasted (Result.map UserPasted >> Result.withDefault Ignore)
            ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
  let
    {title, body} = case (Debug.log "page info state" model.translationState) of
        Decoding fragment -> {title="", body="<decoding...>"}
        Encoding pageInfo -> {title="", body="<encoding...>"}
        Stable (_, pageInfo) -> pageInfo
        NoFragment -> {title="", body=""}
        InvalidFragment -> {title="", body="<invalid fragment>"}
  in
    { title = "EIB" ++ (if String.isEmpty title then "" else (": " ++ title))
    , body =
        [ text "Click here and paste to make a new page."
        , br [] []
        , text "Don't trust the red box any more than you trust the link you clicked on."
        , iframe
            [ srcdoc body
            , style "border" "1px solid red"
            , style "margin" "4em 1% 1% 1%"
            , style "position" "absolute"
            , style "top" "0"
            , style "left" "0"
            , style "width" "98%"
            , style "height" "95%"
            ]
            []
        ]
    }


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]
