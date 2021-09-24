module StructuredText.Html exposing (ItemLinkData, RenderParameters, render)

import Html exposing (Html, a, blockquote, code, em, footer, h1, h2, h3, h4, h5, h6, hr, li, mark, ol, p, pre, span, strong, text, ul)
import Html.Attributes exposing (attribute, class, href, style)
import StructuredText.Types exposing (BlockNode(..), BlockquoteChildNode(..), BlockquoteNode(..), CodeNode(..), HeadingChildNode(..), HeadingLevel(..), HeadingNode(..), InlineItemNode(..), ItemLinkChildNode(..), ItemLinkNode(..), LinkChildNode(..), LinkNode(..), ListChildNode(..), ListItemChildNode(..), ListItemNode(..), ListNode(..), ListStyle(..), Mark(..), ParagraphChildNode(..), ParagraphNode(..), RootChildNode(..), SpanNode(..), StructuredText(..), ThematicBreakNode(..))


type alias RenderParameters a msg =
    { renderBlock : a -> Html msg
    , renderInlineItem : a -> Html msg
    , renderItemLink : ItemLinkData a -> List (Html msg) -> Html msg
    }


type alias ItemLinkData a =
    { item : a
    , meta : List ( String, String )
    }


{-| Render a StructuredText document as elm/html nodes
-}
render : RenderParameters a msg -> StructuredText a -> List (Html msg)
render renderParameters (StructuredText rootChildrenNodes) =
    List.map (renderRootChildNode renderParameters) rootChildrenNodes


renderRootChildNode : RenderParameters a msg -> RootChildNode a -> Html msg
renderRootChildNode renderParameters rootChildNode =
    case rootChildNode of
        RootThematicBreak ThematicBreakNode ->
            renderThematicBreak

        RootCode codeNode ->
            renderCodeNode codeNode

        RootParagraph paragraphNode ->
            renderParagraphNode renderParameters paragraphNode

        RootBlock blockNode ->
            renderBlockNode renderParameters blockNode

        RootHeading headingNode ->
            renderHeadingNode renderParameters headingNode

        RootList listNode ->
            renderListNode renderParameters listNode

        RootBlockquote blockquoteNode ->
            renderBlockquoteNode renderParameters blockquoteNode


renderThematicBreak : Html msg
renderThematicBreak =
    hr [] []


renderCodeNode : CodeNode -> Html msg
renderCodeNode (CodeNode codeNodeData) =
    let
        attributes =
            case codeNodeData.language of
                Just language ->
                    [ class ("lang-" ++ language) ]

                Nothing ->
                    []
    in
    pre []
        [ code attributes [ text codeNodeData.code ]
        ]


renderParagraphNode : RenderParameters a msg -> ParagraphNode a -> Html msg
renderParagraphNode renderParameters (ParagraphNode children) =
    List.map (renderParagraphChildNode renderParameters) children
        |> p []


renderParagraphChildNode : RenderParameters a msg -> ParagraphChildNode a -> Html msg
renderParagraphChildNode renderParameters paragraphChildNode =
    case paragraphChildNode of
        ParagraphSpan spanNode ->
            renderSpanNode spanNode

        ParagraphLink linkNode ->
            renderLinkNode linkNode

        ParagraphInlineItem inlineItemNode ->
            renderInlineItemNode renderParameters inlineItemNode

        ParagraphItemLink itemLinkNode ->
            renderItemLinkNode renderParameters itemLinkNode


renderSpanNode : SpanNode -> Html msg
renderSpanNode (SpanNode spanNodeData) =
    List.foldr
        (\currentMark rendered ->
            case currentMark of
                Strong ->
                    strong [] [ rendered ]

                Code ->
                    code [] [ rendered ]

                Emphasis ->
                    em [] [ rendered ]

                Underline ->
                    span [ style "text-decoration" "underline" ] [ rendered ]

                Strikethrough ->
                    span [ style "text-decoration" "line-through" ] [ rendered ]

                Highlight ->
                    mark [] [ rendered ]
        )
        (text spanNodeData.value)
        spanNodeData.marks


renderLinkNode : LinkNode -> Html msg
renderLinkNode (LinkNode linkNodeData children) =
    let
        metaAttributes =
            List.map (\( name, value ) -> attribute name value) linkNodeData.meta
    in
    List.map renderLinkChildNode children
        |> a (href linkNodeData.url :: metaAttributes)


renderLinkChildNode : LinkChildNode -> Html msg
renderLinkChildNode linkChildNode =
    case linkChildNode of
        LinkSpan spanNode ->
            renderSpanNode spanNode


renderHeadingNode : RenderParameters a msg -> HeadingNode a -> Html msg
renderHeadingNode renderParameters (HeadingNode headNodeData children) =
    List.map (renderHeadingChildNode renderParameters) children
        |> (case headNodeData.level of
                H1 ->
                    h1 []

                H2 ->
                    h2 []

                H3 ->
                    h3 []

                H4 ->
                    h4 []

                H5 ->
                    h5 []

                H6 ->
                    h6 []
           )


renderHeadingChildNode : RenderParameters a msg -> HeadingChildNode a -> Html msg
renderHeadingChildNode renderParameters headingChildNode =
    case headingChildNode of
        HeadingSpan spanNode ->
            renderSpanNode spanNode

        HeadingLink linkNode ->
            renderLinkNode linkNode

        HeadingInlineItem inlineItemNode ->
            renderInlineItemNode renderParameters inlineItemNode

        HeadingItemLink itemLinkNode ->
            renderItemLinkNode renderParameters itemLinkNode


renderBlockquoteNode : RenderParameters a msg -> BlockquoteNode a -> Html msg
renderBlockquoteNode renderParameters (BlockquoteNode blockquoteNodeData children) =
    let
        childrenNodes =
            List.map (renderBlockquoteChildNode renderParameters) children

        attributionNodes =
            case blockquoteNodeData.attribution of
                Just attribution ->
                    [ footer [] [ text attribution ] ]

                Nothing ->
                    []
    in
    blockquote [] (childrenNodes ++ attributionNodes)


renderBlockquoteChildNode : RenderParameters a msg -> BlockquoteChildNode a -> Html msg
renderBlockquoteChildNode renderParameters blockquoteChildNode =
    case blockquoteChildNode of
        BlockquoteParagraph paragraphNode ->
            renderParagraphNode renderParameters paragraphNode


renderListNode : RenderParameters a msg -> ListNode a -> Html msg
renderListNode renderParameters (ListNode listNodeData children) =
    List.map (renderListNodeChild renderParameters) children
        |> (case listNodeData.style of
                Bulleted ->
                    ul []

                Numbered ->
                    ol []
           )


renderListNodeChild : RenderParameters a msg -> ListChildNode a -> Html msg
renderListNodeChild renderParameters listChildNode =
    case listChildNode of
        ListListItem listItemNode ->
            renderListItemNode renderParameters listItemNode


renderListItemNode : RenderParameters a msg -> ListItemNode a -> Html msg
renderListItemNode renderParameters (ListItemNode children) =
    List.map (renderListItemChildNode renderParameters) children
        |> li []


renderListItemChildNode : RenderParameters a msg -> ListItemChildNode a -> Html msg
renderListItemChildNode renderParameters listItemChildNode =
    case listItemChildNode of
        ListItemParagraph paragraphNode ->
            renderParagraphNode renderParameters paragraphNode

        ListItemList listNode ->
            renderListNode renderParameters listNode


renderBlockNode : RenderParameters a msg -> BlockNode a -> Html msg
renderBlockNode renderParameters (BlockNode blockNodeData) =
    renderParameters.renderBlock blockNodeData.itemContent


renderInlineItemNode : RenderParameters a msg -> InlineItemNode a -> Html msg
renderInlineItemNode renderParameters (InlineItemNode inlineItemNodeData) =
    renderParameters.renderInlineItem inlineItemNodeData.itemContent


renderItemLinkNode : RenderParameters a msg -> ItemLinkNode a -> Html msg
renderItemLinkNode renderParameters (ItemLinkNode itemLinkNodeData children) =
    List.map renderItemLinkChildNode children
        |> renderParameters.renderItemLink
            { item = itemLinkNodeData.itemContent
            , meta = itemLinkNodeData.meta
            }


renderItemLinkChildNode : ItemLinkChildNode -> Html msg
renderItemLinkChildNode itemLinkChildNode =
    case itemLinkChildNode of
        ItemLinkSpan spanNode ->
            renderSpanNode spanNode
