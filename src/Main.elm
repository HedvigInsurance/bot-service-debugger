module Main exposing (..)

import Browser exposing (Document)
import Css exposing (backgroundColor, hex)
import Debug exposing (toString)
import Html.Styled exposing (Html, button, div, form, input, text, toUnstyled)
import Html.Styled.Attributes exposing (css, placeholder, value)
import Html.Styled.Events exposing (onInput, onSubmit)
import Http


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = RequestingMemberId { memberId : String }
    | FetchingMessages { memberId : String }
    | HasMessages String
    | ErrorState String


type Msg
    = ChangeMemberId String
    | GotMessages (Result Http.Error String)
    | FetchMessages


init : () -> ( Model, Cmd Msg )
init _ =
    ( RequestingMemberId { memberId = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeMemberId memberId ->
            ( RequestingMemberId { memberId = memberId }, Cmd.none )

        FetchMessages ->
            case model of
                RequestingMemberId { memberId } ->
                    ( FetchingMessages { memberId = memberId }
                    , Http.get
                        { url = "http://localhost:3000/proxy"
                        , expect = Http.expectString GotMessages
                        }
                    )

                _ ->
                    ( ErrorState "Illegal state (expected to be RequestingMemberId but was not", Cmd.none )

        GotMessages result ->
            case result of
                Ok fullText ->
                    ( HasMessages fullText, Cmd.none )

                Err err ->
                    ( ErrorState ("Failed to fetch messages: " ++ toString err), Cmd.none )


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
    case model of
        RequestingMemberId { memberId } ->
            form
                [ onSubmit FetchMessages ]
                [ input
                    [ value memberId
                    , onInput ChangeMemberId
                    , placeholder "123456"
                    ]
                    []
                , button
                    []
                    [ text "Debug 🐞" ]
                ]

        FetchingMessages { memberId } ->
            div []
                [ text ("Fetching messages for " ++ memberId) ]

        HasMessages messages ->
            div []
                [ text messages ]

        ErrorState errorMessage ->
            div
                [ css [ backgroundColor (hex "ff0000") ]
                ]
                [ text errorMessage
                ]
