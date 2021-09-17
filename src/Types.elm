module Types exposing (Node(..), ParagraphChildNode, ParagraphNode(..), SpanNode(..), StructuredText(..))


type StructuredText
    = StructuredText (List Node)


type Node
    = Paragraph ParagraphNode
    | Span SpanNode


type ParagraphNode
    = ParagraphNode (List ParagraphChildNode)


type SpanNode
    = SpanNode String


type ParagraphChildNode
    = SpanParagraphChildNode
    | LinkParagraphChildNode
    | ItemLinkParagraphChildNode
    | InlineItemParagraphChildNode
