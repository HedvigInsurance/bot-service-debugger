port module Main exposing (..)

import Browser exposing (Document)
import Css as ListStyle exposing (BorderStyle, ListStyle, backgroundColor, borderColor, borderStyle, borderWidth, hex, listStyle, margin, padding, px, rgba, solid)
import Debug exposing (toString)
import Html.Styled exposing (Html, button, div, form, input, li, p, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, placeholder, value)
import Html.Styled.Events exposing (onInput, onSubmit)
import Http exposing (header)
import Json.Decode exposing (Decoder, field, int, keyValuePairs, map2, map5, map6, maybe, string)
import Result exposing (Result(..))
import Task


main : Program PersistedModel Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type UiState
    = EnteringMemberId
    | FetchingMessages
    | HasMessages { messages : List BotServiceMessage }
    | ErrorState String


type alias Model =
    { memberId : String
    , state : UiState
    }


type alias PersistedModel =
    { memberId : String }


port storeMemberId : String -> Cmd msg


type Msg
    = ChangeMemberId String
    | GotMessages (Result Http.Error BotServiceMessages)
    | FetchMessages


init : PersistedModel -> ( Model, Cmd Msg )
init flags =
    let
        cmd =
            if not (flags.memberId == "") then
                Task.succeed FetchMessages |> Task.perform identity

            else
                Cmd.none
    in
    ( { memberId = flags.memberId, state = EnteringMemberId }
    , cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeMemberId memberId ->
            ( { model | memberId = memberId }, storeMemberId memberId )

        FetchMessages ->
            ( { model | state = FetchingMessages }
            , Http.request
                { method = "GET"
                , url = "http://localhost:3000/bot-service/messages"
                , expect = Http.expectJson GotMessages decodeMessages
                , headers =
                    [ header "hedvig.token" model.memberId ]
                , body = Http.emptyBody
                , timeout = Just 30000.0
                , tracker = Nothing
                }
            )

        GotMessages result ->
            case result of
                Ok messages ->
                    ( { model | state = HasMessages { messages = List.map Tuple.second messages } }, Cmd.none )

                Err err ->
                    ( { model | state = ErrorState ("Failed to fetch messages: " ++ toString err) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "Bot Service Debugger"
    , body = [ toUnstyled (body model) ]
    }


body : Model -> Html Msg
body model =
    case model.state of
        EnteringMemberId ->
            debugForm model

        FetchingMessages ->
            div []
                [ text ("Fetching messages for " ++ model.memberId) ]

        HasMessages { messages } ->
            div []
                [ debugForm model
                , messageList messages
                ]

        ErrorState errorMessage ->
            div
                [ css [ backgroundColor (hex "ff0000") ]
                ]
                [ debugForm model
                , text errorMessage
                ]


debugForm : Model -> Html Msg
debugForm model =
    form [ onSubmit FetchMessages ]
        [ input
            [ value model.memberId
            , onInput ChangeMemberId
            , placeholder "123456"
            ]
            []
        , button
            []
            [ text "Debug 🐞" ]
        ]


messageList : List BotServiceMessage -> Html Msg
messageList messages =
    ul
        [ css
            [ listStyle ListStyle.none
            , padding (px 0)
            , margin (px 0)
            ]
        ]
        (List.map message messages)


message : BotServiceMessage -> Html Msg
message message_ =
    li
        [ css
            [ padding (px 16)
            , margin (px 8)
            , borderWidth (px 1)
            , borderStyle solid
            , borderColor (rgba 0 0 0 0.1)
            ]
        ]
        [ p [] [ text message_.body.text ]
        ]


type alias BotServiceMessageHeader =
    { messageId : Int
    , fromId : Int
    }


type alias BotServiceMessageBody =
    { type_ : String
    , id : Int
    , text : String
    , imageURL : Maybe String
    , textContentType : Maybe String
    }


type alias BotServiceMessage =
    { globalId : Int
    , id : String
    , header : BotServiceMessageHeader
    , body : BotServiceMessageBody
    , timestamp : String
    , author : Maybe String
    }


type alias BotServiceMessages =
    List ( String, BotServiceMessage )


decodeHeader : Decoder BotServiceMessageHeader
decodeHeader =
    map2 BotServiceMessageHeader
        (field "messageId" int)
        (field "fromId" int)


decodeBody : Decoder BotServiceMessageBody
decodeBody =
    map5 BotServiceMessageBody
        (field "type" string)
        (field "id" int)
        (field "text" string)
        (maybe (field "imageURL" string))
        (maybe (field "textContentType" string))


decodeMessage : Decoder BotServiceMessage
decodeMessage =
    map6 BotServiceMessage
        (field "globalId" int)
        (field "id" string)
        (field "header" decodeHeader)
        (field "body" decodeBody)
        (field "timestamp" string)
        (maybe (field "author" string))


decodeMessages : Decoder (List ( String, BotServiceMessage ))
decodeMessages =
    keyValuePairs decodeMessage
