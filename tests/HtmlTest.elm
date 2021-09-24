module HtmlTest exposing (..)

import Expect exposing (Expectation)
import Html exposing (Html, code, em, hr, img, mark, p, pre, span, strong, text)
import Html.Attributes exposing (alt, class, src, style)
import StructuredText.Html
import StructuredText.Types exposing (BlockNode(..), CodeNode(..), ItemId(..), Mark(..), ParagraphChildNode(..), ParagraphNode(..), RootChildNode(..), SpanNode(..), StructuredText(..), ThematicBreakNode(..))
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


suite : Test
suite =
    describe "The StructuredText.Html module"
        [ describe "decoder"
            [ test "should render an empty document" <|
                \_ ->
                    let
                        input =
                            StructuredText []

                        result =
                            StructuredText.Html.render { renderBlock = always (text "") } input
                    in
                    Expect.equal [] result
            , test "should render a thematic break node" <|
                \_ ->
                    let
                        input =
                            StructuredText [ RootThematicBreak ThematicBreakNode ]

                        result =
                            StructuredText.Html.render { renderBlock = always (text "") } input
                    in
                    Expect.equal [ hr [] [] ] result
            , test "should render a code node" <|
                \_ ->
                    let
                        input =
                            StructuredText
                                [ RootCode
                                    (CodeNode
                                        { code = "main = 1"
                                        , language = Just "elm"
                                        , highlight = []
                                        }
                                    )
                                ]

                        result =
                            StructuredText.Html.render { renderBlock = always (text "") } input
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
                            StructuredText
                                [ RootParagraph
                                    (ParagraphNode
                                        [ ParagraphSpan (SpanNode { value = "Hello", marks = [] }) ]
                                    )
                                ]

                        result =
                            StructuredText.Html.render { renderBlock = always (text "") } input
                    in
                    Expect.equal
                        [ p [] [ span [] [ text "Hello" ] ] ]
                        result
            , test "should render a paragraph with a span containing marks" <|
                \_ ->
                    let
                        input =
                            StructuredText
                                [ RootParagraph
                                    (ParagraphNode
                                        [ ParagraphSpan (SpanNode { value = "Hello", marks = [ Emphasis, Code, Highlight ] })
                                        , ParagraphSpan (SpanNode { value = " World!", marks = [ Underline, Strong, Strikethrough ] })
                                        ]
                                    )
                                ]

                        result =
                            StructuredText.Html.render { renderBlock = always (text "") } input
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
            , test "should render a custom block" <|
                \_ ->
                    let
                        input =
                            StructuredText
                                [ RootBlock
                                    (BlockNode
                                        { itemId = ItemId "58599620"
                                        , itemContent = ImageItem "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg" FullWidth "Elm logo"
                                        }
                                    )
                                ]

                        result =
                            StructuredText.Html.render { renderBlock = renderImageItem } input
                    in
                    Expect.equal
                        [ img [ class "full-width", src "https://www.datocms-assets.com/53557/1628850590-elm-logo.svg", alt "Elm logo" ] []
                        ]
                        result
            ]
        ]
