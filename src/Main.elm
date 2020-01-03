import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Url
import Json.Decode as D
import Json.Encode as E

import Clipboard
import B64Lzma.Translation as Translation exposing (B64Lzma(..))
import B64Lzma.Routes as Routes


-- MAIN

type alias InteropConstants =
    { capturePasteClass : String
    , initAutofocusId : String
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
    | Stable Translation.EncodingRelation
    | TranslationFailed String

type PasteContentType
    = ContentTypeText
    | ContentTypeHtml
    | ContentTypeAuto

type alias Model =
  { key : Nav.Key
  , route : Routes.Route
  , body : BodyState
  , trusted : Bool
  , pasteContentType : PasteContentType
  , interopConstants : InteropConstants
  , showTrustToast : Bool
  }

init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { key = key
            , route = Routes.parseUrl url
            , body = NoFragment
            , trusted = False
            , pasteContentType = ContentTypeAuto
            , interopConstants = flags.interopConstants
            , showTrustToast = True
            }
    in
        update (UrlChanged url) model



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
  | Decoded (Result Translation.Error Translation.EncodingRelation)
  | Encoded (Result Translation.Error Translation.EncodingRelation)
  | UserPasted Clipboard.PastedData
  | TitleAltered String
  | TrustToggled Bool
  | DismissTrustToast
  | PasteContentTypeToggled PasteContentType
  | Ignore

pushOrReplaceRoute : Nav.Key -> Routes.Route -> Routes.Route -> Cmd msg
pushOrReplaceRoute key oldRoute newRoute =
    if (oldRoute == Routes.indexRoute) /= (newRoute == Routes.indexRoute)
        then Nav.pushUrl    key (Routes.toString newRoute)
        else Nav.replaceUrl key (Routes.toString newRoute)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.load (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
        let
            newRoute : Routes.Route
            newRoute = Routes.parseUrl url

            alreadyDecoding : Maybe B64Lzma
            alreadyDecoding =
                case model.body of
                    Stable {encoded} -> Just encoded
                    Decoding encoded -> Just encoded
                    _ -> Nothing

            (newBody, cmd) =
                case (alreadyDecoding, newRoute.encodedBody) of
                    (Just oldEncodedBody, Just newEncodedBody) ->
                        if oldEncodedBody == newEncodedBody
                            then (model.body, Cmd.none)
                            else (Decoding newEncodedBody, Translation.decode newEncodedBody)
                    (Nothing, Just newEncodedBody) ->
                        (Decoding newEncodedBody, Translation.decode newEncodedBody)
                    (_, Nothing) ->
                        (NoFragment, Cmd.none)
        in
            ( { model | route = newRoute
                      , body = newBody
                      }
            , cmd
            )

    Encoded (Err e) ->
        ( { model | body = TranslationFailed <| "Error encoding: " ++ Debug.toString e }
        , Cmd.none
        )
    Encoded (Ok relation) ->
        case model.body of
            Encoding body ->
                if relation.plaintext == body
                    then
                        ( { model | body = Stable relation }
                        , pushOrReplaceRoute model.key model.route
                            { title = getTitle model
                            , encodedBody = Just relation.encoded
                            }
                        )
                    else
                        ( model , Cmd.none )
            _ -> ( model, Cmd.none)

    Decoded (Err e) ->
        ( { model | body = TranslationFailed <| "Error decoding: " ++ Debug.toString e }
        , Cmd.none
        )
    Decoded (Ok relation) ->
        case model.body of
            Decoding encodedBody ->
                if relation.encoded == encodedBody
                    then
                        ( { model | body = Stable relation }
                        , Cmd.none
                        )
                    else ( model , Cmd.none )
            _ -> ( model , Cmd.none )

    UserPasted pastedData ->
        let
            body = case model.pasteContentType of
                ContentTypeText ->
                    case pastedData.plainText of
                        Just raw -> wrapInPre raw
                        Nothing -> ""
                ContentTypeHtml ->
                    case pastedData.html of
                        Just h -> h
                        Nothing ->
                            case pastedData.plainText of
                                Just raw -> raw
                                Nothing -> ""

                ContentTypeAuto ->
                    case pastedData.html of
                        Just h -> h
                        Nothing ->
                            case pastedData.plainText of
                                Just raw -> wrapInPre raw
                                Nothing -> ""
        in
            ( { model | body = Encoding body }
            , Translation.encode body
            )

    TitleAltered title ->
        let
            newRoute =
                model.route
                |> Routes.setTitle (if String.isEmpty title then Nothing else Just title)
        in
            ( { model | route = newRoute }
            , pushOrReplaceRoute model.key model.route newRoute
            )

    TrustToggled trusted ->
        ( { model | trusted = trusted }
        , Cmd.none
        )

    PasteContentTypeToggled mode ->
        ( { model | pasteContentType = mode }
        , Cmd.none
        )

    DismissTrustToast ->
        ( { model | showTrustToast = False }
        , Cmd.none
        )

    Ignore ->
      ( model, Cmd.none )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Translation.decoded Decoded
        , Translation.encoded Encoded
        , Clipboard.userPasted (Result.map UserPasted >> Result.withDefault Ignore)
        ]



-- VIEW

pasteFieldAttrs : String -> List (Attribute Msg)
pasteFieldAttrs capturePasteClass =
    [ onInput (always Ignore)
    , value ""
    -- ^^ HACK: make Elm re-render on input, to keep this box empty
    , class capturePasteClass
    ]

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

getTitle : Model -> Maybe String
getTitle model =
    model.route.title

view : Model -> Browser.Document Msg
view model =
    { title = getTitle model |> Maybe.withDefault "B64Lzma"
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
        [ div [style "width" "30%"] []
        , div [style "width" "40%"] [textarea [ placeholder "Title"
                                              , value (model.route.title |> Maybe.withDefault "")
                                              , style "text-align" "center"
                                              , style "font-weight" "700"
                                              , style "font-size" "1em"
                                              , style "width" "100%"
                                              , style "resize" "none"
                                              , style "border" "0"
                                              , onInput TitleAltered
                                              ]
                                              [] ]
        , div [style "width" "30%", style "color" "gray"]
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
    [ input
        (pasteFieldAttrs model.interopConstants.capturePasteClass ++
            [ placeholder "Paste here to set the page content."
            , style "width" "90%"
            , id model.interopConstants.initAutofocusId
            ])
        []
    , details []
        [ summary []
            [ text "(click for details)"
            ]
        , text "By default, the content-type (html, text, ...) of the pasted data is magically inferred. Use this dropdown to override: "
        , select
            [ onInput (\s -> PasteContentTypeToggled <| case s of
                "ContentTypeAuto" -> ContentTypeAuto
                "ContentTypeHtml" -> ContentTypeHtml
                "ContentTypeText" -> ContentTypeText
                _ -> Debug.todo "impossible"
                )
            ]
            [ option [ value "ContentTypeAuto", selected (model.pasteContentType == ContentTypeAuto) ] [text "(auto)"]
            , option [ value "ContentTypeHtml", selected (model.pasteContentType == ContentTypeHtml) ] [text "HTML"]
            , option [ value "ContentTypeText", selected (model.pasteContentType == ContentTypeText) ] [text "plain text"]
            ]
        ]
    ]


viewBody : Model -> Html Msg
viewBody model =
    let
        centeredAttrs =
            [ style "text-align" "center"
            , style "width" "100%"
            ]
        coloredCenteredAttrs : String -> List (Html.Attribute msg)
        coloredCenteredAttrs color = style "color" color :: centeredAttrs
    in
        case model.body of
            NoFragment ->
                div (coloredCenteredAttrs "gray")
                    [ p [] [text "No content yet; paste your desired content, then share the URL!"]
                    , p [] [a [href "about.html"] [text "(about)"]]
                    ]
            Encoding s ->
                div (coloredCenteredAttrs "gray") [text <| "encoding " ++ (s |> String.length |> toFloat |> (\n -> n/1000) |> round |> String.fromInt) ++ " kB..."]
            Decoding (B64Lzma e) ->
                div (coloredCenteredAttrs "gray") [text <| "decoding " ++ (e |> String.length |> toFloat |> (\n -> n/1000) |> round |> String.fromInt) ++ " kB..."]
            TranslationFailed err ->
                div (coloredCenteredAttrs "purple") [text err]
            Stable {plaintext} ->
                div [ style "text-align" "center", style "width" "100%", style "height" "100%" ]
                    [ if model.showTrustToast
                        then div [style "border" "1px dashed black"]
                            [ text "The stuff in the red box below is no more credible or safe than the link you clicked on to get to this page (or the content you pasted to create this page)."
                            , br [] []
                            , input [ id "trusted-toggle"
                                    , type_ "checkbox"
                                    , value (if model.trusted then "on" else "off")
                                    , onClick (TrustToggled <| not model.trusted)
                                    ]
                                    []
                            , label [for "trusted-toggle"] [text "Allow it to run JavaScript, etc?"]
                            , text " | "
                            , button [onClick DismissTrustToast] [text "Hide warning"]
                            ]
                        else text ""
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
