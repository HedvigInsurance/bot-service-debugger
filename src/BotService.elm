module BotService exposing (..)

import Json.Decode exposing (Decoder, bool, field, int, keyValuePairs, list, map3, map5, map6, maybe, string)
import Json.Encode exposing (Value, encode, object)
import Json.Encode.Extra


type alias BotServiceMessageHeader =
    { messageId : Int
    , fromId : Int
    , timeStamp : Int
    }


type alias BotServiceMessageBody =
    { type_ : String
    , id : Int
    , text : String
    , imageURL : Maybe String
    , textContentType : Maybe String
    , choices : Maybe (List Choice)
    }


type alias Choice =
    { type_ : String
    , selected : Bool
    , text : String
    , value : String
    , clearable : Bool
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
    map3 BotServiceMessageHeader
        (field "messageId" int)
        (field "fromId" int)
        (field "timeStamp" int)


decodeChoice : Decoder Choice
decodeChoice =
    map5 Choice
        (field "type" string)
        (field "selected" bool)
        (field "text" string)
        (field "value" string)
        (field "clearable" bool)


decodeBody : Decoder BotServiceMessageBody
decodeBody =
    map6 BotServiceMessageBody
        (field "type" string)
        (field "id" int)
        (field "text" string)
        (maybe <| field "imageURL" <| string)
        (maybe <| field "textContentType" <| string)
        (maybe <| field "choice" <| list decodeChoice)


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


encodeHeader : BotServiceMessageHeader -> Value
encodeHeader header =
    object
        [ ( "messageId", Json.Encode.int header.messageId )
        , ( "fromId", Json.Encode.int header.fromId )
        , ( "timeStamp", Json.Encode.int header.timeStamp )
        ]


encodeChoices : List Choice -> Value
encodeChoices =
    Json.Encode.list
        (\c ->
            object
                [ ( "type", Json.Encode.string c.type_ )
                , ( "selected", Json.Encode.bool c.selected )
                , ( "text", Json.Encode.string c.text )
                , ( "value", Json.Encode.string c.value )
                , ( "clearable", Json.Encode.bool c.clearable )
                ]
        )


encodeBody : BotServiceMessageBody -> Value
encodeBody body =
    object
        [ ( "type", Json.Encode.string body.type_ )
        , ( "id", Json.Encode.int body.id )
        , ( "text", Json.Encode.string body.text )
        , ( "imageURL", Json.Encode.Extra.maybe Json.Encode.string body.imageURL )
        , ( "textContentType", Json.Encode.Extra.maybe Json.Encode.string body.textContentType )
        , ( "choices", encodeChoices (Maybe.withDefault [] body.choices) )
        ]


encodeMessage : BotServiceMessage -> Value
encodeMessage message =
    object
        [ ( "globalId", Json.Encode.int message.globalId )
        , ( "id", Json.Encode.string message.id )
        , ( "header", encodeHeader message.header )
        , ( "body", encodeBody message.body )
        , ( "timestamp", Json.Encode.string message.timestamp )
        , ( "author", Json.Encode.Extra.maybe Json.Encode.string message.author )
        ]


encodeMessageRaw : BotServiceMessage -> String
encodeMessageRaw message =
    encodeMessage message
        |> encode 2
