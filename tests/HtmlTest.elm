module HtmlTest exposing (ImageFormat(..), ImageItem, emptyRenderer, renderImageItem, renderImageItemLink, suite)

import Expect exposing (Expectation)
import Html exposing (Html, a, blockquote, code, em, footer, h2, hr, img, li, mark, ol, p, pre, span, strong, text, ul)
import Html.Attributes exposing (alt, attribute, class, href, src, style)
import Internal.Types exposing (BlockNode(..), BlockquoteChildNode(..), BlockquoteNode(..), CodeNode(..), Document(..), DocumentItemId(..), HeadingChildNode(..), HeadingLevel(..), HeadingNode(..), InlineItemNode(..), ItemLinkChildNode(..), ItemLinkNode(..), LinkChildNode(..), LinkNode(..), ListChildNode(..), ListItemChildNode(..), ListItemNode(..), ListNode(..), ListStyle(..), Mark(..), ParagraphChildNode(..), ParagraphNode(..), RootChildNode(..), SpanNode(..), ThematicBreakNode(..))
import StructuredText.Html exposing (ItemLinkData, RenderParameters)
import Test exposing (..)


type alias ImageItem =
    { imageUrl : String, format : ImageFormat, alt : String }


type ImageFormat
    = FullWidth
    | ImageWidth


renderImageItem : ImageItem -> Html msg
renderImageItem imageItem =
    let
        className =
            case imageItem.format of
                FullWidth ->
                    "full-width"

                ImageWidth ->
                    "image-width"
    in
    img [ class className, src imageItem.imageUrl, alt imageItem.alt ] []


renderImageItemLink : ItemLinkData ImageItem -> List (Html msg) -> Html msg
renderImageItemLink itemLinkData children =
    let
        attributes =
            List.map (\( name, value ) -> attribute name value) itemLinkData.meta
    in
    a (href itemLinkData.item.imageUrl :: attributes) children


emptyRenderer : RenderParameters a msg
emptyRenderer =
    { renderBlock = always (text "")
    , renderInlineItem = always (text "")
    , renderItemLink = \_ children -> span [] children
    }


suite : Test
suite =
    describe "The StructuredText.Html module"
        [ describe "decoder"
            [ test "should render an empty document" <|
                \_ ->
                    let
                        input =
                            Document []

                        result =
                            StructuredText.Html.render emptyRenderer input
                    in
                    Expect.equal [] result
            , test "should render a thematic break node" <|
                \_ ->
                    let
                        input =
                            Document [ RootThematicBreak ThematicBreakNode ]

                        result =
                            StructuredText.Html.render emptyRenderer input
                    in
                    Expect.equal [ hr [] [] ] result
            , test "should render a code node" <|
                \_ ->
                    let
                        input =
                            Document
                                [ RootCode
                                    (CodeNode
                                        { code = "main = 1"
                                        , language = Just "elm"
                                        , highlight = []
                                        }
                                    )
                                ]

                        result =
                            StructuredText.Html.render emptyRenderer input
                    in
                    Expect.equal
                        [ pre []
                            [ code [ class "lang-elm" ] [ text "main = 1" ]
                            ]
                        ]
                        result
            , test "should render a paragraph node" <|
                \_ ->
                    let
                        input =
                            Document
                                [ RootParagraph
                                    (ParagraphNode
                                        [ ParagraphSpan (SpanNode { value = "Hello", marks = [] }) ]
                                    )
                                ]

                        result =
                            StructuredText.Html.render emptyRenderer input
                    in
                    Expect.equal
                        [ p [] [ text "Hello" ] ]
                        result
            , test "should render a paragraph with a span containing marks" <|
                \_ ->
                    let
                        input =
                            Document
                                [ RootParagraph
                                    (ParagraphNode
                                        [ ParagraphSpan (SpanNode { value = "Hello", marks = [ Emphasis, Code, Highlight ] })
                                        , ParagraphSpan (SpanNode { value = " World!", marks = [ Underline, Strong, Strikethrough ] })
                                        ]
                                    )
                                ]

                        result =
                            StructuredText.Html.render emptyRenderer input
                    in
                    Expect.equal
                        [ p []
                            [ em []
                                [ code []
                                    [ mark [] [ text "Hello" ]
                                    ]
                                ]
                            , span [ style "text-decoration" "underline" ]
                                [ strong []
                                    [ span [ style "text-decoration" "line-through" ] [ text " World!" ]
                                    ]
                                ]
                            ]
                        ]
                        result
            , test "should render a paragraph with a link" <|
                \_ ->
                    let
                        input =
                            Document
                                [ RootParagraph
                                    (ParagraphNode
                                        [ ParagraphLink
                                            (LinkNode { url = "https://elm-lang.org", meta = [ ( "target", "_blank" ) ] }
                                                [ LinkSpan (SpanNode { value = "Hello", marks = [] }) ]
                                            )
                                        ]
                                    )
                                ]

                        result =
                            StructuredText.Html.render emptyRenderer input
                    in
                    Expect.true "HTML is not rendered as expected" <|
                        [ p []
                            [ a [ href "https://elm-lang.org", attribute "target" "_blank" ] [ text "Hello" ]
                            ]
                        ]
                            == result
            , test "should render a heading node" <|
                \_ ->
                    let
                        input =
                            Document
                                [ RootHeading
                                    (HeadingNode { level = H2 } [ HeadingSpan (SpanNode { value = "Hello", marks = [] }) ])
                                ]

                        result =
                            StructuredText.Html.render emptyRenderer input
                    in
                    Expect.equal
                        [ h2 [] [ text "Hello" ] ]
                        result
            , test "should render a blockquote node" <|
                \_ ->
                    let
                        input =
                            Document
                                [ RootBlockquote
                                    (BlockquoteNode { attribution = Just "Myself" }
                                        [ BlockquoteParagraph
                                            (ParagraphNode
                                                [ ParagraphSpan (SpanNode { value = "Hello", marks = [] })
                                                ]
                                            )
                                        ]
                                    )
                                ]

                        result =
                            StructuredText.Html.render emptyRenderer input
                    in
                    Expect.equal
                        [ blockquote []
                            [ p [] [ text "Hello" ]
                            , footer [] [ text "Myself" ]
                            ]
                        ]
                        result
            , test "should render a numbered list node" <|
                \_ ->
                    let
                        input =
                            Document
                                [ RootList
                                    (ListNode { style = Numbered }
                                        [ ListListItem
                                            (ListItemNode
                                                [ ListItemParagraph
                                                    (ParagraphNode
                                                        [ ParagraphSpan (SpanNode { value = "Hello", marks = [] })
                                                        ]
                                                    )
                                                ]
                                            )
                                        ]
                                    )
                                ]

                        result =
                            StructuredText.Html.render emptyRenderer input
                    in
                    Expect.equal
                        [ ol []
                            [ li []
                                [ p [] [ text "Hello" ]
                                ]
                            ]
                        ]
                        result
            , test "should render a bulleted list node" <|
                \_ ->
                    let
                        input =
                            Document
                                [ RootList
                                    (ListNode { style = Bulleted }
                                        [ ListListItem
                                            (ListItemNode
                                                [ ListItemParagraph
                                                    (ParagraphNode
                                                        [ ParagraphSpan (SpanNode { value = "Hello", marks = [] })
                                                        ]
                                                    )
                                                ]
                                            )
                                        ]
                                    )
                                ]

                        result =
                            StructuredText.Html.render emptyRenderer input
                    in
                    Expect.equal
                        [ ul []
                            [ li []
                                [ p [] [ text "Hello" ]
                                ]
                            ]
                        ]
                        result
            , test "should render a custom block" <|
                \_ ->
                    let
                        input =
                            Document
                                [ RootBlock
                                    (BlockNode
                                        { itemId = DocumentItemId "58599620"
                                        , itemContent = ImageItem "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth "Elm logo"
                                        }
                                    )
                                ]

                        result =
                            StructuredText.Html.render { emptyRenderer | renderBlock = renderImageItem } input
                    in
                    Expect.equal
                        [ img [ class "full-width", src "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg", alt "Elm logo" ] []
                        ]
                        result
            , test "should render an inline item" <|
                \_ ->
                    let
                        input =
                            Document
                                [ RootParagraph
                                    (ParagraphNode
                                        [ ParagraphInlineItem
                                            (InlineItemNode
                                                { itemId = DocumentItemId "58599620"
                                                , itemContent = ImageItem "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth "Elm logo"
                                                }
                                            )
                                        ]
                                    )
                                ]

                        result =
                            StructuredText.Html.render { emptyRenderer | renderInlineItem = renderImageItem } input
                    in
                    Expect.equal
                        [ p []
                            [ img [ class "full-width", src "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg", alt "Elm logo" ] []
                            ]
                        ]
                        result
            , test "should render an item link" <|
                \_ ->
                    let
                        input =
                            Document
                                [ RootParagraph
                                    (ParagraphNode
                                        [ ParagraphItemLink
                                            (ItemLinkNode
                                                { itemId = DocumentItemId "58599620"
                                                , itemContent = ImageItem "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth "Elm logo"
                                                , meta = [ ( "target", "_blank" ) ]
                                                }
                                                [ ItemLinkSpan (SpanNode { value = "Hello", marks = [] }) ]
                                            )
                                        ]
                                    )
                                ]

                        result =
                            StructuredText.Html.render { emptyRenderer | renderItemLink = renderImageItemLink } input
                    in
                    Expect.equal
                        [ p []
                            [ a [ href "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg", attribute "target" "_blank" ] [ text "Hello" ]
                            ]
                        ]
                        result
            ]
        ]
