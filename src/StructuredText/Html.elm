module StructuredText.Html exposing (render)

import Html exposing (Html, code, em, hr, mark, p, pre, span, strong, text)
import Html.Attributes exposing (class, style)
import StructuredText.Types exposing (BlockNode(..), CodeNode(..), Mark(..), ParagraphChildNode(..), ParagraphNode(..), RootChildNode(..), SpanNode(..), StructuredText(..), ThematicBreakNode(..))


type alias RenderParameters a msg =
    { renderBlock : a -> Html msg
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
            renderParagraphNode paragraphNode

        RootBlock blockNode ->
            renderBlockNode renderParameters blockNode

        _ ->
            text ""


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


renderParagraphNode : ParagraphNode a -> Html msg
renderParagraphNode (ParagraphNode children) =
    List.map renderParagraphChildNode children
        |> p []


renderParagraphChildNode : ParagraphChildNode a -> Html msg
renderParagraphChildNode paragraphChildNode =
    case paragraphChildNode of
        ParagraphSpan spanNode ->
            renderSpanNode spanNode

        _ ->
            text ""


renderSpanNode : SpanNode -> Html msg
renderSpanNode (SpanNode spanNodeData) =
    case spanNodeData.marks of
        [] ->
            span [] [ text spanNodeData.value ]

        _ ->
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


renderBlockNode : RenderParameters a msg -> BlockNode a -> Html msg
renderBlockNode renderParameters (BlockNode blockNodeData) =
    renderParameters.renderBlock blockNodeData.itemContent
