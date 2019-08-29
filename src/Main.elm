port module Main exposing (..)

import BotService
import Browser exposing (Document)
import Css as ListStyle exposing (BorderStyle, Color, ColorValue, Display, ListStyle, backgroundColor, border3, color, column, display, displayFlex, flexDirection, fontSize, height, hex, inlineBlock, listStyle, margin, padding, pct, px, rgba, row, solid, width)
import Debug exposing (toString)
import Html.Styled exposing (Attribute, Html, button, div, form, h3, input, li, text, textarea, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, placeholder, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Http exposing (header)
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


backOfficeBaseMessage : String -> String
backOfficeBaseMessage memberId =
    """{
  "memberId": \"""" ++ memberId ++ """",
  "msg": "",
  "userId": "you@hedvig.com"
}"""


type UiState
    = EnteringMemberId
    | FetchingMessages
    | HasMessages { messages : List BotService.BotServiceMessage }
    | ErrorState String


type ResponseState
    = NotResponding
    | Responding
    | Responded
    | ResponseFailed String


type alias Response =
    { message : Maybe String
    , state : ResponseState
    }


type alias Model =
    { memberId : String
    , intent : String
    , state : UiState
    , response : Response
    , backOfficeMessage : Response
    }


type alias PersistedModel =
    { memberId : String }


port storeMemberId : String -> Cmd msg


type Msg
    = ChangeMemberId String
    | ChangeIntent String
    | GotMessages (Result Http.Error BotService.BotServiceMessages)
    | FetchMessages
    | FetchMessagesInBackground
    | PrepMessageResponse Int
    | ShowBackOfficeForm
    | ChangeResponse String
    | ChangeBackOfficeResponse String
    | Respond String
    | RespondBackOffice String
    | GotResponse (Result Http.Error ())
    | GotBackOfficeResponse (Result Http.Error ())


init : PersistedModel -> ( Model, Cmd Msg )
init flags =
    let
        cmd =
            if not (flags.memberId == "") then
                Task.succeed FetchMessages |> Task.perform identity

            else
                Cmd.none
    in
    ( { memberId = flags.memberId
      , intent = "onboarding"
      , state = EnteringMemberId
      , response = { message = Nothing, state = NotResponding }
      , backOfficeMessage = { message = Nothing, state = NotResponding }
      }
    , cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeMemberId memberId ->
            ( { model | memberId = memberId }, storeMemberId memberId )

        ChangeIntent intent ->
            ( { model | intent = intent }, Cmd.none )

        FetchMessages ->
            ( { model | state = FetchingMessages }, fetchMessages model.memberId model.intent )

        FetchMessagesInBackground ->
            ( model, fetchMessages model.memberId model.intent )

        GotMessages result ->
            case result of
                Ok messages ->
                    ( { model
                        | state =
                            HasMessages
                                { messages = List.map Tuple.second messages }
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | state = ErrorState ("Failed to fetch messages: " ++ toString err) }, Cmd.none )

        PrepMessageResponse globalMessageId ->
            case model.state of
                HasMessages { messages } ->
                    let
                        response =
                            model.response
                    in
                    ( { model
                        | response =
                            { response
                                | message =
                                    Just
                                        (findMessage globalMessageId messages
                                            |> Maybe.map BotService.encodeMessageRaw
                                            |> Maybe.withDefault ""
                                        )
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | state = ErrorState "Illegal state" }, Cmd.none )

        ChangeResponse response ->
            ( { model | response = { message = Just response, state = NotResponding } }, Cmd.none )

        ChangeBackOfficeResponse response ->
            ( { model | backOfficeMessage = { message = Just response, state = NotResponding } }, Cmd.none )

        ShowBackOfficeForm ->
            ( { model | backOfficeMessage = { message = Just (backOfficeBaseMessage model.memberId), state = NotResponding } }, Cmd.none )

        Respond response ->
            ( { model | response = { message = Just response, state = Responding } }
            , postResponse model.memberId response
            )

        RespondBackOffice response ->
            ( { model | backOfficeMessage = { message = Just response, state = Responding } }
            , postBackOfficeMessage model.memberId response
            )

        GotResponse result ->
            case result of
                Ok _ ->
                    ( { model
                        | response = { message = model.response.message, state = Responded }
                      }
                    , Task.succeed FetchMessagesInBackground |> Task.perform identity
                    )

                Err e ->
                    ( { model
                        | response =
                            { message = model.response.message
                            , state = ResponseFailed (toString e)
                            }
                      }
                    , Cmd.none
                    )

        GotBackOfficeResponse result ->
            case result of
                Ok _ ->
                    ( { model
                        | backOfficeMessage = { message = model.backOfficeMessage.message, state = Responded }
                      }
                    , Task.succeed FetchMessagesInBackground |> Task.perform identity
                    )

                Err e ->
                    ( { model
                        | backOfficeMessage =
                            { message = model.backOfficeMessage.message
                            , state = ResponseFailed (toString e)
                            }
                      }
                    , Cmd.none
                    )


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
            debugForm "Debug 🐞" model

        FetchingMessages ->
            div []
                [ text ("Fetching messages for " ++ model.memberId) ]

        HasMessages { messages } ->
            div []
                [ debugForm "Reload ♻️" model
                , button [ onClick ShowBackOfficeForm ] [ text "Back-office message 🏢" ]
                , case model.backOfficeMessage.message of
                    Nothing ->
                        text ""

                    Just _ ->
                        responseForm
                            "070646"
                            (RespondBackOffice <| responseMessageOrEmpty model.backOfficeMessage)
                            ChangeBackOfficeResponse
                            model.backOfficeMessage
                , case model.response.message of
                    Nothing ->
                        text ""

                    Just _ ->
                        responseForm
                            "009175"
                            (Respond <| responseMessageOrEmpty model.response)
                            ChangeResponse
                            model.response
                , messageList messages
                ]

        ErrorState errorMessage ->
            div
                [ css [ backgroundColor (hex "ff0000") ]
                ]
                [ debugForm "Debug 🐞" model
                , text errorMessage
                ]


fetchMessages : String -> String -> Cmd Msg
fetchMessages memberId intent =
    Http.request
        { method = "GET"
        , url = "http://localhost:3000/bot-service/messages?intent=" ++ intent
        , expect = Http.expectJson GotMessages BotService.decodeMessages
        , headers =
            [ header "hedvig.token" memberId ]
        , body = Http.emptyBody
        , timeout = Just 30000.0
        , tracker = Nothing
        }


postResponse : String -> String -> Cmd Msg
postResponse memberId messageBody =
    Http.request
        { method = "POST"
        , url = "http://localhost:3000/bot-service/response"
        , expect = Http.expectWhatever GotResponse
        , headers =
            [ header "hedvig.token" memberId, header "Accept" "application/json;charset=utf-8" ]
        , body = Http.stringBody "application/json" messageBody
        , timeout = Just 30000.0
        , tracker = Nothing
        }


postBackOfficeMessage : String -> String -> Cmd Msg
postBackOfficeMessage memberId messageBody =
    Http.request
        { method = "POST"
        , url = "http://localhost:3000/bot-service/_/messages/addmessage"
        , expect = Http.expectWhatever GotBackOfficeResponse
        , headers =
            [ header "Authorization" memberId, header "Accept" "application/json;charset=utf-8" ]
        , body = Http.stringBody "application/json" messageBody
        , timeout = Just 30000.0
        , tracker = Nothing
        }


debugForm : String -> Model -> Html Msg
debugForm label model =
    form [ onSubmit FetchMessages ]
        [ input
            [ value model.memberId
            , onInput ChangeMemberId
            , placeholder "123456"
            ]
            []
        , input
            [ value model.intent
            , onInput ChangeIntent
            , placeholder "intent"
            ]
            []
        , button
            []
            [ text label ]
        ]


responseMessageOrEmpty : Response -> String
responseMessageOrEmpty response =
    Maybe.withDefault "" response.message


responseForm : String -> Msg -> (String -> Msg) -> Response -> Html Msg
responseForm backgroundColorHex submitMsg changeMsg response =
    form
        [ onSubmit submitMsg
        , css
            [ backgroundColor (hex backgroundColorHex)
            , padding (px 16)
            ]
        ]
        [ h3 [] [ text "Respond" ]
        , div
            [ css
                [ displayFlex
                , flexDirection row
                ]
            ]
            [ messageTextarea [ onInput changeMsg ] (responseMessageOrEmpty response)
            , div
                []
                [ case response.state of
                    NotResponding ->
                        button [ type_ "submit" ] [ text "Send response" ]

                    Responded ->
                        div []
                            [ button [ type_ "submit" ] [ text "Send response" ]
                            , div [ css [ color (hex "1be9b6") ] ] [ text "Successfully responded" ]
                            ]

                    Responding ->
                        div [] [ text "Loading..." ]

                    ResponseFailed e ->
                        div []
                            [ button [ type_ "submit" ]
                                [ text "Send response" ]
                            , div
                                [ css [ color (hex "ff8a80") ] ]
                                [ text ("Error: " ++ toString e) ]
                            ]
                ]
            ]
        ]


messageList : List BotService.BotServiceMessage -> Html Msg
messageList messages =
    ul
        [ css
            [ listStyle ListStyle.none
            , padding (px 0)
            , margin (px 0)
            ]
        ]
        (messages
            |> List.sortBy (\m -> m.header.timeStamp)
            |> List.reverse
            |> List.map message
        )


message : BotService.BotServiceMessage -> Html Msg
message message_ =
    let
        encodedMessage =
            BotService.encodeMessageRaw message_
    in
    li
        [ css
            [ displayFlex
            , flexDirection column
            , padding (px 16)
            , margin (px 8)
            , border3 (px 1) solid (rgba 255 255 255 0.2)
            , backgroundColor
                (if message_.header.fromId == 1 then
                    rgba 0 0 0 0

                 else
                    hex "323222"
                )
            ]
        ]
        [ div []
            [ div [ css [ fontSize (px 24) ] ] [ text message_.body.text ]
            , div
                [ css
                    [ displayFlex
                    , flexDirection row
                    ]
                ]
                [ messageTextarea [] encodedMessage
                , div [] [ prepResponseButton message_.globalId ]
                ]
            ]
        ]


messageTextarea : List (Attribute Msg) -> String -> Html Msg
messageTextarea attributes messageText =
    textarea
        ([ css
            [ padding (px 8)
            , border3 (px 1) solid (rgba 255 255 255 0.2)
            , display inlineBlock
            , height
                (messageText
                    |> String.split "\n"
                    |> List.length
                    |> toFloat
                    |> (\f -> (f + 2) * 16 * 1.5)
                    |> px
                )
            , width (pct 50)
            ]
         , value messageText
         ]
            ++ attributes
        )
        []


prepResponseButton : Int -> Html Msg
prepResponseButton globalMessageId =
    button [ onClick (PrepMessageResponse globalMessageId) ] [ text "Prep response" ]


findMessage : Int -> List BotService.BotServiceMessage -> Maybe BotService.BotServiceMessage
findMessage globalMessageId messages =
    messages
        |> List.filter (\m -> m.globalId == globalMessageId)
        |> List.head
