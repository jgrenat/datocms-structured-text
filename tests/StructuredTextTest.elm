module StructuredTextTest exposing (..)

import Expect exposing (Expectation)
import Json.Decode as Decode exposing (Decoder, Error(..))
import StructuredText
import Test exposing (..)
import Types exposing (BlockId(..), BlockNode(..), BlockquoteChildNode(..), BlockquoteNode(..), CodeNode(..), HeadingChildNode(..), HeadingLevel(..), HeadingNode(..), LinkChildNode(..), LinkNode(..), ListChildNode(..), ListItemChildNode(..), ListItemNode(..), ListNode(..), ListStyle(..), Mark(..), ParagraphChildNode(..), ParagraphNode(..), RootChildNode(..), SpanNode(..), StructuredText(..), ThematicBreakNode(..))


type alias ImageBlock =
    { imageUrl : String, format : ImageFormat }


type ImageFormat
    = FullWidth
    | ImageWidth


imageBlockDecoder : Decoder ImageBlock
imageBlockDecoder =
    Decode.map2 ImageBlock
        (Decode.at [ "image", "url" ] Decode.string)
        (Decode.field "fullWidth" imageFormatDecoder)


imageFormatDecoder : Decoder ImageFormat
imageFormatDecoder =
    Decode.bool
        |> Decode.map
            (\isFullWidth ->
                if isFullWidth then
                    FullWidth

                else
                    ImageWidth
            )


suite : Test
suite =
    describe "The StructuredText module"
        [ describe "decoder"
            -- Nest as many descriptions as you like.
            [ test "should decode an empty document" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.equal (Ok (StructuredText [])) result
            , test "should reject a document with invalid schema type" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dats", "document": { "type": "root", "children": [] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    case result of
                        Err (Field "schema" (Failure "Invalid schema type" _)) ->
                            Expect.pass

                        _ ->
                            Expect.fail "There should a schema type error"
            , test "should reject a document with invalid root node" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "roto", "children": [] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    case result of
                        Err (Field "document" (Field "type" (Failure "Invalid root type" _))) ->
                            Expect.pass

                        _ ->
                            Expect.fail "There should a root node type error"
            , test "should decode a document with a thematic break" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [ { "type": "thematicBreak" } ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.equal (Ok (StructuredText [ RootThematicBreak ThematicBreakNode ])) result
            , test "should decode a document with a simple code" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                            {
                                "type": "code",
                                "code": "type ParagraphChildNode = ParagraphChildNode String"
                            }
                            ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (StructuredText
                                [ RootCode
                                    (CodeNode
                                        { code = "type ParagraphChildNode = ParagraphChildNode String"
                                        , language = Nothing
                                        , highlight = []
                                        }
                                    )
                                ]
                            )
                        )
                        result
            , test "should decode a document with an empty paragraph" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [ { "type": "paragraph", "children": [] } ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.equal (Ok (StructuredText [ RootParagraph (ParagraphNode []) ])) result
            , test "should decode a document with a paragraph containing a span with no mark" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                        { "type": "paragraph"
                        , "children": [
                            { "type": "span", "value" : "SpanValue" }
                        ]
                        } ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (StructuredText
                                [ RootParagraph
                                    (ParagraphNode [ ParagraphSpan (SpanNode { value = "SpanValue", marks = [] }) ])
                                ]
                            )
                        )
                        result
            , test "should decode a document with a paragraph containing a span with marks" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                        { "type": "paragraph"
                        , "children": [
                            { "type": "span"
                            , "value" : "SpanValue2"
                            , "marks": ["strong", "code", "emphasis", "underline", "strikethrough", "highlight"]
                            }
                        ]
                        } ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (StructuredText
                                [ RootParagraph
                                    (ParagraphNode
                                        [ ParagraphSpan
                                            (SpanNode { value = "SpanValue2", marks = [ Strong, Code, Emphasis, Underline, Strikethrough, Highlight ] })
                                        ]
                                    )
                                ]
                            )
                        )
                        result
            , test "should decode a document with a paragraph containing a link with no meta and no children" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                        { "type": "paragraph"
                        , "children": [
                            { "type": "link"
                            , "url": "https://www.any-url.com"
                            , "children": []
                            }
                        ]
                        } ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (StructuredText
                                [ RootParagraph
                                    (ParagraphNode
                                        [ ParagraphLink
                                            (LinkNode
                                                { url = "https://www.any-url.com"
                                                , meta = []
                                                }
                                                []
                                            )
                                        ]
                                    )
                                ]
                            )
                        )
                        result
            , test "should fail decoding a document with a paragraph containing a link with no url" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                        { "type": "paragraph"
                        , "children": [
                            { "type": "link"
                            , "children": []
                            }
                        ]
                        } ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.err result
            , test "should decode a document with a paragraph containing a link with meta and children" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                        { "type": "paragraph"
                        , "children": [
                            { "type": "link"
                            , "url": "https://www.any-url.com"
                            , "meta": [ {"id": "meta1", "value": "value1"} ]
                            , "children": [ {"type": "span", "value": "SpanValue"} ]
                            }
                        ]
                        } ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (StructuredText
                                [ RootParagraph
                                    (ParagraphNode
                                        [ ParagraphLink
                                            (LinkNode
                                                { url = "https://www.any-url.com"
                                                , meta = [ ( "meta1", "value1" ) ]
                                                }
                                                [ LinkSpan (SpanNode { value = "SpanValue", marks = [] }) ]
                                            )
                                        ]
                                    )
                                ]
                            )
                        )
                        result
            , test "should decode a document with a heading" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                        { "type": "heading"
                        , "level": 2
                        , "children": [
                            {"type": "span", "value": "SpanValue", "marks": ["emphasis"] }
                            , {"type": "link", "url": "URL", "children": [] }
                        ]
                        }
                        ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (StructuredText
                                [ RootHeading
                                    (HeadingNode { level = H2 }
                                        [ HeadingSpan (SpanNode { value = "SpanValue", marks = [ Emphasis ] })
                                        , HeadingLink (LinkNode { url = "URL", meta = [] } [])
                                        ]
                                    )
                                ]
                            )
                        )
                        result
            , test "should fail decoding a document with a heading with an invalid level" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                        { "type": "heading"
                        , "level": 7
                        , "children": [ {"type": "span", "value": "SpanValue", "marks": ["emphasis"] } ]
                        }
                        ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.err result
            , test "should fail decoding a document with a heading with a missing level" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                        { "type": "heading"
                        , "children": [ {"type": "span", "value": "SpanValue", "marks": ["emphasis"] } ]
                        }
                        ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.err result
            , test "should decode a document with a list and no children" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                        { "type": "list"
                        , "style": "bulleted"
                        , "children": []
                        }
                        ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.equal (Ok (StructuredText [ RootList (ListNode { style = Bulleted } []) ])) result
            , test "should fail decoding a document with a list with invalid style" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                        { "type": "list"
                        , "style": "bluleted"
                        , "children": []
                        }
                        ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.err result
            , test "should fail decoding a document with a list with missing style" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                        { "type": "list"
                        , "children": []
                        }
                        ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.err result
            , test "should decode a document with a list and complex children" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                        { "type": "list"
                        , "style": "bulleted"
                        , "children": [ { "type": "listItem", "children": [
                            { "type": "paragraph", "children": [ { "type": "span", "value": "SpanValue" } ] }
                            , { "type": "list", "style": "numbered", "children": [] }
                        ] } ]
                        }
                        ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (StructuredText
                                [ RootList
                                    (ListNode { style = Bulleted }
                                        [ ListListItem
                                            (ListItemNode
                                                [ ListItemParagraph (ParagraphNode [ ParagraphSpan (SpanNode { value = "SpanValue", marks = [] }) ])
                                                , ListItemList (ListNode { style = Numbered } [])
                                                ]
                                            )
                                        ]
                                    )
                                ]
                            )
                        )
                        result
            , test "should decode a document with a blockquote with no children and no attribution" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                        { "type": "blockquote"
                        , "children": []
                        }
                        ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.equal
                        (Ok (StructuredText [ RootBlockquote (BlockquoteNode { attribution = Nothing } []) ]))
                        result
            , test "should decode a document with a blockquote with children and attribution" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                        { "type": "blockquote"
                        , "attribution": "Thanks to myself"
                        , "children": [ {"type": "paragraph", "children": [ {"type": "link", "url": "URL", "children": []} ]} ]
                        }
                        ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (StructuredText
                                [ RootBlockquote
                                    (BlockquoteNode { attribution = Just "Thanks to myself" }
                                        [ BlockquoteParagraph
                                            (ParagraphNode
                                                [ ParagraphLink (LinkNode { url = "URL", meta = [] } [])
                                                ]
                                            )
                                        ]
                                    )
                                ]
                            )
                        )
                        result
            , test "should decode a document with a custom block node" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                              {
                                "item": "58599620",
                                "type": "block"
                              }
                            ] } }"""

                        result =
                            Decode.decodeString
                                (StructuredText.decoder
                                    [ ( BlockId "58599620", ImageBlock "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth )
                                    ]
                                )
                                input
                    in
                    Expect.equal
                        (Ok
                            (StructuredText
                                [ RootBlock
                                    (BlockNode (BlockId "58599620")
                                        (ImageBlock "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth)
                                    )
                                ]
                            )
                        )
                        result
            , test "should fail to decode an image block if the block is not provided" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                              {
                                "item": "58599620",
                                "type": "block"
                              }
                            ] } }"""

                        result =
                            Decode.decodeString (StructuredText.decoder []) input
                    in
                    case result of
                        Err (Field "document" (Field "children" (Index 0 (Failure "Unable to find a matching block with ID 58599620" _)))) ->
                            Expect.pass

                        _ ->
                            Expect.fail "There should be an error specifying that the block was not found"
            ]
        , describe "blockDecoder"
            [ test "should decode an image block" <|
                \_ ->
                    let
                        input =
                            """{
                               "image": {
                                 "url": "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg"
                               },
                               "fullWidth": true,
                               "id": "58599620"
                             }"""

                        result =
                            Decode.decodeString (StructuredText.blockDecoder imageBlockDecoder) input
                    in
                    Expect.equal
                        (Ok
                            ( BlockId "58599620", ImageBlock "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth )
                        )
                        result
            ]
        ]
