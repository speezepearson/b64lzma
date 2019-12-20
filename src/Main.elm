import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Url
import Json.Decode as D
import Json.Encode as E

import B64Lzma exposing (B64Lzma(..))
import Clipboard
import Ittybitty.Fragments as Fragments exposing (Fragment)
import Ittybitty.PageInfo exposing (PageInfo, EncodingRelation)


replaceUrl : Nav.Key -> Url.Url -> Cmd msg
replaceUrl key url =
    Nav.replaceUrl key (Debug.log "pushing url" (Url.toString url))

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


type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , title : String
  , body : String
  -- , errors: List String
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    startDecoding url {key=key, url=url, title="", body=""}

startDecoding : Url.Url -> Model -> ( Model, Cmd Msg )
startDecoding url model =
    case Fragments.parseUrl url of
        Nothing ->
            ( { model | url=url, title="", body="" }
            , Cmd.none
            )
        Just (Err _) ->
            ( { model | url=url }
            , Cmd.none
            )
        Just (Ok fragment) ->
            ( { model | url=url, title=Fragments.getTitle fragment }
            , B64Lzma.decode <| Fragments.getEncodedBody fragment
            )


-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | Decoded B64Lzma.EncodingRelation
  | Encoded B64Lzma.EncodingRelation
  | UserPasted String
  | TitleAltered String
  | Ignore -- TODO: have better error handling


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
        startDecoding url model

    Encoded {plaintext, encoded} ->
        ( model
        , replaceUrl model.key (Fragments.addToUrl (Just (Fragments.build model.title encoded)) model.url)
        )

    Decoded {plaintext, encoded} ->
        ( { model | body=plaintext }
        , Cmd.none
        )

    UserPasted body ->
        ( model
        , B64Lzma.encode body
        )

    TitleAltered title ->
        ( { model | title = title }
        , replaceUrl model.key (Fragments.mapUrl (\f -> Fragments.build title (Fragments.getEncodedBody f)) model.url)
        )

    Ignore ->
      ( model, Cmd.none )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ B64Lzma.decoded (Result.map Decoded >> Result.withDefault Ignore)
        , B64Lzma.encoded (Result.map Encoded >> Result.withDefault Ignore)
        , Clipboard.userPasted (Result.map UserPasted >> Result.withDefault Ignore)
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = if String.isEmpty model.title then "Elm-Ittybitty" else model.title
    , body =
        [ div
            [ style "width" "100%"
            , style "display" "flex"
            , style "flex-direction" "row"
            , style "justify-content" "space-between"
            ]
            [ div [style "width" "30%"] [text "Don't trust the red box any more than you trust the link you clicked on."]
            , div [style "width" "40%"] [textarea [ placeholder "Title"
                                                  , value model.title
                                                  , style "text-align" "center"
                                                  , style "font-weight" "700"
                                                  , style "font-size" "1em"
                                                  , style "width" "100%"
                                                  , style "resize" "none"
                                                  , style "border" "0"
                                                  , Html.Events.onInput TitleAltered
                                                  ]
                                                  [] ]
            , div [style "width" "30%", style "color" "gray", style "text-align" "right"]
                [ input [ value ""
                        , style "width" "100%"
                        , placeholder "Paste here to set the page content."
                        , id "body-setter"
                        ]
                        []
                ]
            ]
        , iframe
            [ srcdoc model.body
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
