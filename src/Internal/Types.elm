module Internal.Types exposing (..)


type Document a
    = Document (List (RootChildNode a))


type RootChildNode a
    = RootParagraph (ParagraphNode a)
    | RootHeading (HeadingNode a)
    | RootList (ListNode a)
    | RootCode CodeNode
    | RootBlockquote (BlockquoteNode a)
    | RootThematicBreak ThematicBreakNode
    | RootBlock (BlockNode a)


type ParagraphChildNode a
    = ParagraphSpan SpanNode
    | ParagraphLink LinkNode
    | ParagraphInlineItem (InlineItemNode a)
    | ParagraphItemLink (ItemLinkNode a)


type HeadingChildNode a
    = HeadingSpan SpanNode
    | HeadingLink LinkNode
    | HeadingInlineItem (InlineItemNode a)
    | HeadingItemLink (ItemLinkNode a)


type ListChildNode a
    = ListListItem (ListItemNode a)


type ListItemChildNode a
    = ListItemParagraph (ParagraphNode a)
    | ListItemList (ListNode a)


type LinkChildNode
    = LinkSpan SpanNode


type BlockquoteChildNode a
    = BlockquoteParagraph (ParagraphNode a)


type ParagraphNode a
    = ParagraphNode (List (ParagraphChildNode a))


type HeadingNode a
    = HeadingNode { level : HeadingLevel } (List (HeadingChildNode a))


type ListNode a
    = ListNode { style : ListStyle } (List (ListChildNode a))


type ListItemNode a
    = ListItemNode (List (ListItemChildNode a))


type CodeNode
    = CodeNode
        { code : String
        , language : Maybe String
        , highlight : List Int
        }


type BlockquoteNode a
    = BlockquoteNode { attribution : Maybe String } (List (BlockquoteChildNode a))


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


type InlineItemNode a
    = InlineItemNode
        { itemId : ItemId
        , itemContent : a
        }


type ItemLinkNode a
    = ItemLinkNode
        { itemId : ItemId
        , itemContent : a
        , meta : List ( String, String )
        }
        (List ItemLinkChildNode)


type ItemLinkChildNode
    = ItemLinkSpan SpanNode


type BlockNode a
    = BlockNode
        { itemId : ItemId
        , itemContent : a
        }


type ItemId
    = ItemId String


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
