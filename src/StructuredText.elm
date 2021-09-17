module StructuredText exposing (decoder)

import Json.Decode as Decode exposing (Decoder)
import Types exposing (Node(..), ParagraphChildNode, ParagraphNode(..), SpanNode(..), StructuredText(..))


decoder : Decode.Decoder StructuredText
decoder =
    Decode.field "schema" schemaDecoder
        |> Decode.andThen (\() -> Decode.field "document" documentDecoder)


schemaDecoder : Decoder ()
schemaDecoder =
    Decode.string
        |> Decode.andThen
            (\schemaString ->
                if schemaString == "dast" then
                    Decode.succeed ()

                else
                    Decode.fail "Invalid schema"
            )


documentDecoder : Decoder StructuredText
documentDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\schemaString ->
                if schemaString == "root" then
                    Decode.succeed ()

                else
                    Decode.fail "Invalid root type"
            )
        |> Decode.andThen (\() -> childrenDecoder)
        |> Decode.map StructuredText


childrenDecoder : Decoder (List Node)
childrenDecoder =
    Decode.field "children" (Decode.list nodeDecoder)


nodeDecoder : Decoder Node
nodeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "paragraph" ->
                        Decode.map Paragraph paragraphDecoder

                    "span" ->
                        Decode.field "value" Decode.string
                            |> Decode.map SpanNode
                            |> Decode.map Span

                    _ ->
                        Decode.fail ("Unknown or unhandled node type: " ++ typeString)
            )


paragraphDecoder : Decoder ParagraphNode
paragraphDecoder =
    Decode.map ParagraphNode (Decode.list paragraphChildNodeDecoder)



-- TODO: double-check generated code


paragraphChildNodeDecoder : Decoder ParagraphChildNode
paragraphChildNodeDecoder =
    let
        get id =
            case id of
                "span" ->
                    Decode.succeed (SpanParagraphChildNode (SpanNode ""))

                _ ->
                    Decode.fail ("unknown value for ParagraphChildNode: " ++ id)
    in
    Decode.string |> Decode.andThen get
