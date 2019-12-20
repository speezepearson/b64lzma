import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser exposing (fragment)
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
        translationState : TranslationState
        translationState =
            case Fragments.parseUrl url of
                Nothing -> NoFragment
                Just (Err _) -> InvalidFragment
                Just (Ok fragment) -> Decoding fragment

        cmd = case translationState of
            Decoding fragment -> Fragments.getEncodedBody fragment |> B64Lzma.b64LzmaDecode
            _ -> Cmd.none
    in
        ( { model | url=url, translationState=translationState }
        , cmd
        )

-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | B64LzmaRelationDetermined B64Lzma.EncodingRelation
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

    B64LzmaRelationDetermined relation ->
        case model.translationState of
            Encoding pageInfo ->
                if (relation.plaintext == pageInfo.body)
                    then ( model
                         , relation.encoded
                           |> Fragments.build ""
                           |> (\fragment -> Fragments.addToUrl (Just fragment) model.url)
                           |> pushUrl model.key
                         )
                    else let
                            _ = Debug.log "got a relation we didn't expect" (msg, model)
                         in
                            (model, Cmd.none)
            Decoding fragment ->
                if (relation.encoded == Fragments.getEncodedBody fragment)
                    then ( { model | translationState = Stable (fragment, { title=Fragments.getTitle fragment, body=relation.plaintext })}
                         , Cmd.none
                         )
                    else let
                            _ = Debug.log "got a relation we didn't expect" (msg, model)
                         in
                            (model, Cmd.none)
            _ -> let _ = Debug.log "got an EncodingRelation when not expecting" (msg, model) in (model, Cmd.none)

    UserPasted body ->
        ( { model | translationState = Encoding {title="", body=body} }
        , B64Lzma.b64LzmaEncode body
        )

    Ignore ->
      ( model, Cmd.none )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ B64Lzma.b64LzmaResult (Result.map B64LzmaRelationDetermined >> Result.withDefault Ignore)
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
    { title = "Elm-Ittybitty"
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
