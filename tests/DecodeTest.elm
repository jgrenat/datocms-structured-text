module DecodeTest exposing (ImageFormat(..), ImageItem, suite)

import Expect exposing (Expectation)
import Internal.Types exposing (BlockNode(..), BlockquoteChildNode(..), BlockquoteNode(..), CodeNode(..), Document(..), DocumentItemId(..), HeadingChildNode(..), HeadingLevel(..), HeadingNode(..), InlineItemNode(..), ItemLinkChildNode(..), ItemLinkNode(..), LinkChildNode(..), LinkNode(..), ListChildNode(..), ListItemChildNode(..), ListItemNode(..), ListNode(..), ListStyle(..), Mark(..), ParagraphChildNode(..), ParagraphNode(..), RootChildNode(..), SpanNode(..), ThematicBreakNode(..))
import Json.Decode as Decode exposing (Decoder, Error(..))
import StructuredText.Decode
import Test exposing (..)


type alias ImageItem =
    { imageUrl : String, format : ImageFormat }


type ImageFormat
    = FullWidth
    | ImageWidth


suite : Test
suite =
    describe "The StructuredText.Decode module"
        [ describe "decoder"
            [ test "should decode an empty document" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [] } }"""

                        result =
                            Decode.decodeString (StructuredText.Decode.decoder []) input
                    in
                    Expect.equal (Ok (Document [])) result
            , test "should reject a document with invalid schema type" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dats", "document": { "type": "root", "children": [] } }"""

                        result =
                            Decode.decodeString (StructuredText.Decode.decoder []) input
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
                    in
                    Expect.equal (Ok (Document [ RootThematicBreak ThematicBreakNode ])) result
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (Document
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
                    in
                    Expect.equal (Ok (Document [ RootParagraph (ParagraphNode []) ])) result
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (Document
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (Document
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (Document
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (Document
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (Document
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
                    in
                    Expect.equal (Ok (Document [ RootList (ListNode { style = Bulleted } []) ])) result
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (Document
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
                    in
                    Expect.equal
                        (Ok (Document [ RootBlockquote (BlockquoteNode { attribution = Nothing } []) ]))
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
                    in
                    Expect.equal
                        (Ok
                            (Document
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
                                (StructuredText.Decode.decoder
                                    [ ( DocumentItemId "58599620", ImageItem "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth )
                                    ]
                                )
                                input
                    in
                    Expect.equal
                        (Ok
                            (Document
                                [ RootBlock
                                    (BlockNode
                                        { itemId = DocumentItemId "58599620"
                                        , itemContent = ImageItem "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth
                                        }
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
                            Decode.decodeString (StructuredText.Decode.decoder []) input
                    in
                    case result of
                        Err (Field "document" (Field "children" (Index 0 (Failure "Unable to find a matching item with ID 58599620" _)))) ->
                            Expect.pass

                        Err _ ->
                            Expect.fail "There was an error but not the one expected. There should be an error specifying that the block was not found."

                        _ ->
                            Expect.fail "There should be an error specifying that the block was not found"
            , test "should decode a document with a heading containing an inline item" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                              {
                                "type": "heading",
                                "level": 4,
                                "children": [
                                  {
                                    "item": "58599620",
                                    "type": "inlineItem"
                                  }
                                ]
                              }
                            ] } }"""

                        result =
                            Decode.decodeString
                                (StructuredText.Decode.decoder
                                    [ ( DocumentItemId "58599620"
                                      , ImageItem "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth
                                      )
                                    ]
                                )
                                input
                    in
                    Expect.equal
                        (Ok
                            (Document
                                [ RootHeading
                                    (HeadingNode { level = H4 }
                                        [ HeadingInlineItem
                                            (InlineItemNode
                                                { itemId = DocumentItemId "58599620"
                                                , itemContent = ImageItem "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth
                                                }
                                            )
                                        ]
                                    )
                                ]
                            )
                        )
                        result
            , test "should decode a document with a paragraph containing an inline item" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                              {
                                "type": "paragraph",
                                "children": [
                                  {
                                    "item": "58599620",
                                    "type": "inlineItem"
                                  }
                                ]
                              }
                            ] } }"""

                        result =
                            Decode.decodeString
                                (StructuredText.Decode.decoder
                                    [ ( DocumentItemId "58599620"
                                      , ImageItem "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth
                                      )
                                    ]
                                )
                                input
                    in
                    Expect.equal
                        (Ok
                            (Document
                                [ RootParagraph
                                    (ParagraphNode
                                        [ ParagraphInlineItem
                                            (InlineItemNode
                                                { itemId = DocumentItemId "58599620"
                                                , itemContent = ImageItem "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth
                                                }
                                            )
                                        ]
                                    )
                                ]
                            )
                        )
                        result
            , test "should decode a document with a paragraph containing an item link" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                              {
                                "type": "paragraph",
                                "children": [
                                  {
                                    "item": "58599620",
                                    "type": "itemLink",
                                    "children": [{
                                      "type": "span",
                                      "value": "inline link"
                                    }]
                                  }
                                ]
                              }
                            ] } }"""

                        result =
                            Decode.decodeString
                                (StructuredText.Decode.decoder
                                    [ ( DocumentItemId "58599620"
                                      , ImageItem "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth
                                      )
                                    ]
                                )
                                input
                    in
                    Expect.equal
                        (Ok
                            (Document
                                [ RootParagraph
                                    (ParagraphNode
                                        [ ParagraphItemLink
                                            (ItemLinkNode
                                                { itemId = DocumentItemId "58599620"
                                                , itemContent = ImageItem "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth
                                                , meta = []
                                                }
                                                [ ItemLinkSpan (SpanNode { value = "inline link", marks = [] }) ]
                                            )
                                        ]
                                    )
                                ]
                            )
                        )
                        result
            , test "should decode a document with a heading containing an item link" <|
                \_ ->
                    let
                        input =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                              {
                                "type": "heading",
                                "level": 5,
                                "children": [
                                  {
                                    "item": "58599620",
                                    "type": "itemLink",
                                    "meta": [ { "id": "metaId", "value": "metaValue" } ],
                                    "children": [{
                                      "type": "span",
                                      "value": "inline link"
                                    }]
                                  }
                                ]
                              }
                            ] } }"""

                        result =
                            Decode.decodeString
                                (StructuredText.Decode.decoder
                                    [ ( DocumentItemId "58599620"
                                      , ImageItem "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth
                                      )
                                    ]
                                )
                                input
                    in
                    Expect.equal
                        (Ok
                            (Document
                                [ RootHeading
                                    (HeadingNode { level = H5 }
                                        [ HeadingItemLink
                                            (ItemLinkNode
                                                { itemId = DocumentItemId "58599620"
                                                , itemContent = ImageItem "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth
                                                , meta = [ ( "metaId", "metaValue" ) ]
                                                }
                                                [ ItemLinkSpan (SpanNode { value = "inline link", marks = [] }) ]
                                            )
                                        ]
                                    )
                                ]
                            )
                        )
                        result
            ]
        ]
