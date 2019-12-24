import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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

type alias InteropConstants =
    { ignorePasteClass : String
    }

type alias Flags =
    { interopConstants : InteropConstants
    }

main : Program Flags Model Msg
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

type PasteMode
    = PasteText
    | PasteHtml
    | PasteAuto

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , title : String
  , body : String
  , trusted : Bool
  , pasteMode : PasteMode
  , interopConstants : InteropConstants
  -- , errors: List String
  }

init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    startDecoding url
        { key = key
        , url = url
        , title = ""
        , body = ""
        , trusted = False
        , pasteMode = PasteAuto
        , interopConstants = flags.interopConstants
        }

startDecoding : Url.Url -> Model -> ( Model, Cmd Msg )
startDecoding url model =
    case Fragments.parseUrl url of
        Nothing ->
            ( { model | url=url }
            , Cmd.none
            )
        Just (Err e) ->
            Debug.todo <| "invalid fragment in URL: " ++ Url.toString url
        Just (Ok fragment) ->
            ( { model | url=url, title=Fragments.getTitle fragment }
            , B64Lzma.decode <| Fragments.getEncodedBody fragment
            )

getEncodedBody : Model -> Maybe B64Lzma
getEncodedBody model =
    model.url
    |> Fragments.parseUrl
    |> Maybe.andThen Result.toMaybe
    |> Maybe.map Fragments.getEncodedBody


-- adapted from https://github.com/marcosh/elm-html-to-unicode/blob/1.0.3/src/ElmEscapeHtml.elm
-- less efficient for simplicity; this is not in any tight loops at time of writing
htmlEscape : String -> String
htmlEscape s =
    s
    |> String.replace "&" "&amp;"
    |> String.replace "<" "&lt;"
    |> String.replace ">" "&gt;"
    |> String.replace "\"" "&quot;"
    |> String.replace "'" "&#39;"

wrapInPre : String -> String
wrapInPre s =
    "<pre>" ++ htmlEscape s ++ "</pre>"


-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | Decoded B64Lzma.EncodingRelation
  | Encoded B64Lzma.EncodingRelation
  | UserPasted Clipboard.PastedData
  | TitleAltered String
  | TrustToggled Bool
  | PasteModeToggled PasteMode
  | Ignore


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
        , if plaintext == model.body
            then replaceUrl model.key (Fragments.addToUrl (Just (Fragments.build model.title encoded)) model.url)
            else Cmd.none
        )

    Decoded {plaintext, encoded} ->
        ( if Just encoded == getEncodedBody model
            then { model | body=plaintext }
            else model
        , Cmd.none
        )

    UserPasted pastedData ->
        let
            plainTextModeBodyOrWarn : () -> String
            plainTextModeBodyOrWarn () =
                case pastedData.plainText of
                    Just raw -> wrapInPre raw
                    Nothing -> -- TODO: improve errors
                        Debug.log
                            ("warning: no text/plain data in " ++ Debug.toString pastedData)
                            ""
            body : String
            body = case model.pasteMode of
                PasteText ->
                    plainTextModeBodyOrWarn ()
                PasteHtml ->
                    case pastedData.html of
                        Just h -> h
                        Nothing -> -- TODO: improve errors
                            Debug.log
                                ("warning: no text/html data in " ++ Debug.toString pastedData ++ "; falling back to text/plain")
                                (plainTextModeBodyOrWarn ())

                PasteAuto ->
                    case pastedData.html of
                        Just h -> h
                        Nothing -> plainTextModeBodyOrWarn ()
        in
            ( { model | body = body }
            , B64Lzma.encode body
            )

    TitleAltered title ->
        ( { model | title = title }
        , replaceUrl model.key (Fragments.mapUrl (\f -> Fragments.build title (Fragments.getEncodedBody f)) model.url)
        )

    TrustToggled trusted ->
        ( { model | trusted = trusted }
        , Cmd.none
        )

    PasteModeToggled mode ->
        ( { model | pasteMode = mode }
        , Cmd.none
        )

    Ignore ->
      ( model, Cmd.none )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ B64Lzma.decoded (Result.map Decoded >> Result.mapError (Debug.log "error parsing decoded response") >> Result.withDefault Ignore)
        , B64Lzma.encoded (Result.map Encoded >> Result.mapError (Debug.log "error parsing encoded response") >> Result.withDefault Ignore)
        , Clipboard.userPasted (Result.map UserPasted >> Result.withDefault Ignore)
        ]



-- VIEW

fullpage : List (Html msg) -> Html msg
fullpage contents =
    div
        [ style "position" "absolute"
        , style "left" "0"
        , style "top" "0"
        , style "width" "100%"
        , style "height" "100%"
        ]
        contents

view : Model -> Browser.Document Msg
view model =
    { title = if String.isEmpty model.title then "Elm-Ittybitty" else model.title
    , body =
        [ fullpage
            [ viewHeader model
            , viewBody model
            ]
        ]
    }

viewHeader : Model -> Html Msg
viewHeader model =
    div
        [ style "width" "100%"
        , style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-between"
        ]
        [ div [style "width" "30%"]
            [ text "Don't trust the red box any more than you trust the link you clicked on."
            , br [] []
            , input [ id "trusted-toggle"
                    , type_ "checkbox"
                    , value (if model.trusted then "on" else "off")
                    , onClick (TrustToggled <| not model.trusted)
                    ]
                    []
            , label [for "trusted-toggle"] [text "Allow scripts, etc?"]
            ]
        , div [style "width" "40%"] [textarea [ placeholder "Title"
                                              , value model.title
                                              , class model.interopConstants.ignorePasteClass
                                              , style "text-align" "center"
                                              , style "font-weight" "700"
                                              , style "font-size" "1em"
                                              , style "width" "100%"
                                              , style "resize" "none"
                                              , style "border" "0"
                                              , onInput TitleAltered
                                              ]
                                              [] ]
        , div [style "width" "30%", style "color" "gray", style "text-align" "right"]
            (viewPasteInfo model)
        ]

viewRadio : {id : String, label : List (Html msg), checked : Bool, msg : msg} -> Html msg
viewRadio args =
    span []
        [ input [ type_ "radio"
                , id args.id
                , checked args.checked
                , onClick args.msg
                ]
                []
        , label [for args.id] args.label
        ]

viewPasteInfo : Model -> List (Html Msg)
viewPasteInfo model =
    [ text "Paste anywhere (outside red box) to set content."
    , br [] []
    , text "Type: "
    , viewRadio
        { id="set-paste-mode-Text"
        , label=[text "text"]
        , checked=(model.pasteMode == PasteText)
        , msg=(PasteModeToggled PasteText)
        }
    , text " "
    , viewRadio
        { id="set-paste-mode-Html"
        , label=[text "html"]
        , checked=(model.pasteMode == PasteHtml)
        , msg=(PasteModeToggled PasteHtml)
        }
    , text " "
    , viewRadio
        { id="set-paste-mode-Auto"
        , label=[text "auto"]
        , checked=(model.pasteMode == PasteAuto)
        , msg=(PasteModeToggled PasteAuto)
        }
    ]


viewBody : Model -> Html Msg
viewBody model =
    iframe
        [ srcdoc (model.body ++ (if model.trusted then "" else " ")) -- XXX: hack to reload iframe to evade cached permissions
        , sandbox (if model.trusted then "allow-scripts allow-modals" else "")
        , style "border" "1px solid red"
        , style "margin" "1%"
        , style "width" "98%"
        , style "height" "95%"
        ]
        []
