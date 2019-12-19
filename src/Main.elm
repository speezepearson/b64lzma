import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser exposing (fragment)
import Json.Decode as D
import Json.Encode as E

import Ports
import Types exposing (Fragment, PageInfo, EncodingRelation, nilEncodingRelation)


decodeFragmentFromUrl : Url.Url -> Cmd Msg
decodeFragmentFromUrl url =
    Maybe.withDefault Cmd.none
        <| Maybe.map Ports.decodeFragment url.fragment

pushUrl : Nav.Key -> Url.Url -> Cmd msg
pushUrl key url =
    Nav.pushUrl key (Debug.log "pushing url" (Url.toString url))

setFragment : Fragment -> Url.Url -> Url.Url
setFragment fragment url = { url | fragment=Just fragment }


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


type SavedState = Decoding Fragment | Encoding PageInfo | Stable EncodingRelation

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , savedState : SavedState
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    startDecoding url { key=key, url=url, savedState=Stable nilEncodingRelation }

startDecoding : Url.Url -> Model -> ( Model, Cmd Msg )
startDecoding url model =
    case url.fragment of
        Nothing ->
            ( { model | url = url, savedState = Stable nilEncodingRelation }
            , Cmd.none
            )
        Just fragment ->
            ( { model | url = url, savedState = Decoding fragment }
            , Ports.decodeFragment fragment
            )

-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | FragmentDecoded EncodingRelation
  | PageInfoEncoded EncodingRelation
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
        case (url.fragment, model.savedState) of
            (Just fragment, Stable relation) ->
                if fragment == relation.fragment
                    then (model, Cmd.none)
                    else startDecoding url model
            _ -> startDecoding url model

    FragmentDecoded relation ->
      ( { model | savedState = Stable relation }
      , Cmd.none
      )

    PageInfoEncoded relation ->
        ( { model | savedState = Stable relation }
        , pushUrl model.key (setFragment relation.fragment model.url)
        )

    UserPasted body ->
      let
        pageInfo : PageInfo
        pageInfo = {title="", body=body}
      in
        ( { model | savedState = Encoding pageInfo }
        , Ports.encodePageInfo (Debug.log "encoding" pageInfo)
        )

    Ignore ->
      ( model, Cmd.none )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Ports.fragmentDecoded (Result.map FragmentDecoded >> Result.withDefault Ignore)
    , Ports.pageInfoEncoded (Result.map PageInfoEncoded >> Result.withDefault Ignore)
    , Ports.userPasted (Result.map UserPasted >> Result.withDefault Ignore)
    ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
  let
    {title, body} = case (Debug.log "page info state" model.savedState) of
        Decoding fragment -> {title="", body="<decoding...>"}
        Encoding pageInfo -> {title="", body="<encoding...>"}
        Stable {pageInfo} -> pageInfo
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
