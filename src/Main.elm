import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser exposing (fragment)
import Json.Decode as D
import Json.Encode as E

import Ports
import Types exposing (Fragment, PageInfo)


decodeFragmentFromUrl : Url.Url -> Cmd Msg
decodeFragmentFromUrl url =
    Maybe.withDefault Cmd.none
        <| Maybe.map Ports.decodeFragment url.fragment


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


type PageInfoState = Decoding | Decoded PageInfo

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , pageInfoState : PageInfoState
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( { key = key
    , url = url
    , pageInfoState = Decoding
    }
  , decodeFragmentFromUrl url
  )


-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | FragmentDecoded Ports.EncodingRelation
  | PageInfoEncoded Ports.EncodingRelation
  | UserPasted String
  | Ignore -- TODO: have better error handling


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Debug.log "pushing url" (Url.toString url)) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = (Debug.log "set url" url), pageInfoState = Decoding }
      , decodeFragmentFromUrl url
      )

    FragmentDecoded {fragment, pageInfo} ->
      ( { model | pageInfoState = Decoded pageInfo }
      , Cmd.none
      )

    PageInfoEncoded {fragment, pageInfo} ->
      let
        url = let u=model.url in {u | fragment=Just fragment}
      in
        ( { model | url = url}
        , Nav.pushUrl model.key (Debug.log "pushing url" (Url.toString url))
        )

    UserPasted body ->
      let
        pageInfo : PageInfo
        pageInfo = {title="", body=body}
      in
        ( { model | pageInfoState = Decoded pageInfo }
        , Ports.encodePageInfo pageInfo
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
    (title, body) = case (Debug.log "page info state" model.pageInfoState) of
        Decoding -> ("", "")
        Decoded info -> (info.title, info.body)
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
