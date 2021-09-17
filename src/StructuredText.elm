module StructuredText exposing (decoder)

import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder)
import Types exposing (StructuredText)


decoder : Decoder StructuredText
decoder =
    Decode.succeed StructuredText


toHtml : StructuredText -> Html msg
toHtml structuredText =
    text ""
