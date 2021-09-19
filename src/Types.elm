module Types exposing (..)


type StructuredText
    = StructuredText (List RootChildNode)


type RootChildNode
    = RootParagraph ParagraphNode
    | RootHeading HeadingNode
    | RootList ListNode
    | RootCode CodeNode
    | RootBlockquote BlockquoteNode
    | RootThematicBreak ThematicBreakNode


type ParagraphChildNode
    = ParagraphSpan SpanNode
    | ParagraphLink LinkNode


type HeadingChildNode
    = HeadingSpan SpanNode
    | HeadingLink LinkNode


type ListChildNode
    = ListListItem ListItemNode


type ListItemChildNode
    = ListItemParagraph ParagraphNode
    | ListItemList ListNode


type LinkChildNode
    = LinkSpan SpanNode


type BlockquoteChildNode
    = BlockquoteParagraph ParagraphNode


type ParagraphNode
    = ParagraphNode (List ParagraphChildNode)


type HeadingNode
    = HeadingNode { level : HeadingLevel } (List HeadingChildNode)


type ListNode
    = ListNode { style : ListStyle } (List ListChildNode)


type ListItemNode
    = ListItemNode (List ListItemChildNode)


type CodeNode
    = CodeNode
        { code : String
        , language : Maybe String
        , highlight : List Int
        }


type BlockquoteNode
    = BlockquoteNode { attribution : Maybe String } (List BlockquoteChildNode)


type ThematicBreakNode
    = ThematicBreakNode


type SpanNode
    = SpanNode { value : String, marks : List Mark }


type LinkNode
    = LinkNode
        { url : String
        , meta : List ( String, String )
        }
        (List LinkChildNode)


type Mark
    = Strong
    | Code
    | Emphasis
    | Underline
    | Strikethrough
    | Highlight


type HeadingLevel
    = H1
    | H2
    | H3
    | H4
    | H5
    | H6


type ListStyle
    = Bulleted
    | Numbered
