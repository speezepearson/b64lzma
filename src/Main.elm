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

type BodyState
    = NoFragment
    | Encoding String
    | Decoding B64Lzma
    | Stable B64Lzma.EncodingRelation

type PasteContentType
    = ContentTypeText
    | ContentTypeHtml
    | ContentTypeAuto

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , title : String
  , body : BodyState
  , trusted : Bool
  , pasteContentType : PasteContentType
  , interopConstants : InteropConstants
  , errors: List String
  }

init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    startDecoding url
        { key = key
        , url = url
        , title = ""
        , body = NoFragment
        , trusted = False
        , pasteContentType = ContentTypeAuto
        , interopConstants = flags.interopConstants
        , errors = []
        }

startDecoding : Url.Url -> Model -> ( Model, Cmd Msg )
startDecoding url model =
    case Fragments.parseUrl url of
        Nothing ->
            ( { model | url = url, body = NoFragment }
            , Cmd.none
            )
        Just (Err e) ->
            ( { model | url = url, errors = ("invalid fragment: " ++ Debug.toString e) :: model.errors }
            , Cmd.none
            )
        Just (Ok fragment) ->
            let encodedBody = Fragments.getEncodedBody fragment
            in
                ( { model | url = url, body = Decoding encodedBody, title=Fragments.getTitle fragment }
                , B64Lzma.decode encodedBody
                )


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
  | Decoded (Result B64Lzma.Error B64Lzma.EncodingRelation)
  | Encoded (Result B64Lzma.Error B64Lzma.EncodingRelation)
  | UserPasted Clipboard.PastedData
  | TitleAltered String
  | TrustToggled Bool
  | PasteContentTypeToggled PasteContentType
  | DismissErrors
  | Ignore

pushOrReplaceFragment : Nav.Key -> Fragment -> Url.Url -> Cmd msg
pushOrReplaceFragment key fragment url =
    let
        newUrl : Url.Url
        newUrl = Fragments.addToUrl (Just fragment) url

        pushOrReplace : Nav.Key -> String -> Cmd msg
        pushOrReplace =
            if url.fragment == Nothing
                then Nav.pushUrl
                else Nav.replaceUrl
    in
        pushOrReplace key (Url.toString newUrl)

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
        startDecoding (Debug.log "navigated to" url) model

    Encoded (Err e) ->
        ( { model | errors = ("Error encoding: " ++ Debug.toString e) :: model.errors }
        , Cmd.none
        )
    Encoded (Ok relation) ->
        case model.body of
            Encoding body ->
              if relation.plaintext == body
                then
                    ( { model | body = Stable relation }
                    , pushOrReplaceFragment model.key (Fragments.build model.title relation.encoded) model.url
                    )
                else
                    ( model , Cmd.none )
            _ -> ( model , Cmd.none )

    Decoded (Err e) ->
        ( { model | errors = ("Error decoding: " ++ Debug.toString e) :: model.errors }
        , Cmd.none
        )
    Decoded (Ok relation) ->
        case model.body of
            Decoding encodedBody ->
                if relation.encoded == encodedBody
                    then
                        ( { model | body = Stable relation, errors=[] }
                        , Cmd.none
                        )
                    else ( model , Cmd.none )
            _ -> ( model , Cmd.none )

    UserPasted pastedData ->
        let
            (body, errors) = case model.pasteContentType of
                ContentTypeText ->
                    case pastedData.plainText of
                        Just raw -> (wrapInPre raw, [])
                        Nothing -> ("", ["no plain text data in paste"])
                ContentTypeHtml ->
                    case pastedData.html of
                        Just h -> (h, [])
                        Nothing ->
                            case pastedData.plainText of
                                Just raw -> (wrapInPre raw, ["no html data in paste; fell back to plain text"])
                                Nothing -> ("", ["no html or plain text data in paste"])

                ContentTypeAuto ->
                    case pastedData.html of
                        Just h -> (h, [])
                        Nothing ->
                            case pastedData.plainText of
                                Just raw -> (wrapInPre raw, [])
                                Nothing -> ("", ["no html or plain text data in paste"])
        in
            ( { model | body = Encoding body, errors = errors++model.errors }
            , B64Lzma.encode body
            )

    TitleAltered title ->
        ( { model | title = title }
        , Nav.replaceUrl model.key (Url.toString (Fragments.mapUrl (\f -> Fragments.build title (Fragments.getEncodedBody f)) model.url))
        )

    TrustToggled trusted ->
        ( { model | trusted = trusted }
        , Cmd.none
        )

    PasteContentTypeToggled mode ->
        ( { model | pasteContentType = mode }
        , Cmd.none
        )

    DismissErrors ->
        ( { model | errors = [] }
        , Cmd.none
        )

    Ignore ->
      ( model, Cmd.none )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ B64Lzma.decoded Decoded
        , B64Lzma.encoded Encoded
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
            , viewErrors model
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
        [ div [style "width" "30%"] []
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
    [ text "Paste to set content."
    , br [] []
    , select
        [ onInput (\s -> PasteContentTypeToggled <| case s of
            "ContentTypeAuto" -> ContentTypeAuto
            "ContentTypeHtml" -> ContentTypeHtml
            "ContentTypeText" -> ContentTypeText
            _ -> Debug.todo "impossible"
            )
        ]
        [ option [ value "ContentTypeAuto", selected (model.pasteContentType == ContentTypeAuto) ] [text "Auto"]
        , option [ value "ContentTypeHtml", selected (model.pasteContentType == ContentTypeHtml) ] [text "Html"]
        , option [ value "ContentTypeText", selected (model.pasteContentType == ContentTypeText) ] [text "Text"]
        ]
    ]


viewErrors : Model -> Html Msg
viewErrors model =
    if List.isEmpty model.errors
        then text ""
        else div [style "color" "purple", style "margin" "5px", style "border" "1px solid purple"]
            [ text "Errors:"
            , ul [] (List.map (\s -> li [] [text s]) model.errors)
            , button [ onClick DismissErrors ] [text "Dismiss"]
            ]


viewBody : Model -> Html Msg
viewBody model =
    let
        grayCentered contents = div [ style "text-align" "center", style "width" "100%", style "color" "gray" ] contents
    in
        case model.body of
            NoFragment -> grayCentered [text "No content yet; try pasting something, then share the URL!"]
            Encoding _ -> grayCentered [text "encoding..."]
            Decoding _ -> grayCentered [text "decoding..."]
            Stable {plaintext} ->
                div [ style "text-align" "center", style "width" "100%" ]
                    [ text "Don't trust this red box any more than you trust the link you clicked / content you pasted."
                    , br [] []
                    , input [ id "trusted-toggle"
                            , type_ "checkbox"
                            , value (if model.trusted then "on" else "off")
                            , onClick (TrustToggled <| not model.trusted)
                            ]
                            []
                    , label [for "trusted-toggle"] [text "Allow scripts, etc?"]
                    , iframe
                        [ srcdoc (plaintext ++ (if model.trusted then "" else " ")) -- XXX: hack to reload iframe to evade cached permissions
                        , sandbox (if model.trusted then "allow-scripts allow-modals" else "")
                        , style "border" "1px solid red"
                        , style "margin" "1%"
                        , style "width" "98%"
                        , style "height" "95%"
                        ]
                        []
                    ]
