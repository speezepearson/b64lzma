import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser exposing (fragment)
import Json.Decode as D
import Json.Encode as E

import Ports


decodeFragmentFromUrl : Url.Url -> Cmd Msg
decodeFragmentFromUrl url =
    case url.fragment of
        Nothing -> Cmd.none
        Just fragment -> Ports.decodeFragment (E.string fragment)


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
  , loadedFragment : Maybe String
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( { key=key
    , url=url
    , loadedFragment=Nothing
    }
  , Ports.decodeFragment <| E.string <| Maybe.withDefault "" url.fragment
  )


-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | FragmentDecoded String
  | SetFragment String


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
      ( { model | url = url, loadedFragment=Nothing }
      , decodeFragmentFromUrl url
      )

    FragmentDecoded s ->
      ( { model | loadedFragment = Just s }
      , Cmd.none
      )

    SetFragment s ->
      ( model
      , Nav.pushUrl model.key (Url.toString (let u=model.url in {u|fragment=Just s}))
      )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Ports.fragmentDecoded (\enc -> case D.decodeValue D.string enc of
        Ok s -> FragmentDecoded (Debug.log "ok" s)
        Err e -> FragmentDecoded (Debug.toString (Debug.log "err" e)))
    , Ports.desiredFragmentSet (\enc -> case D.decodeValue D.string enc of
        Ok s -> SetFragment (Debug.log "setting fragment" s)
        Err e -> Debug.todo "oh no")
    ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Elm-Ittybitty"
  , body =
      [ text "Click here and paste to make a new page."
      , br [] []
      , text "Don't trust the red box any more than you trust the link you clicked on."
      , iframe
          [ srcdoc (model.loadedFragment |> Maybe.withDefault "loading...")
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
