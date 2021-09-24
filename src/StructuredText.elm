module StructuredText exposing (blockDecoder, decoder, toHtml)

import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import List.Extra as List
import Types exposing (BlockId(..), BlockNode(..), BlockquoteChildNode(..), BlockquoteNode(..), CodeNode(..), HeadingChildNode(..), HeadingLevel(..), HeadingNode(..), LinkChildNode(..), LinkNode(..), ListChildNode(..), ListItemChildNode(..), ListItemNode(..), ListNode(..), ListStyle(..), Mark(..), ParagraphChildNode(..), ParagraphNode(..), RootChildNode(..), SpanNode(..), StructuredText(..), ThematicBreakNode(..))


{-| Decodes a DatoCMS Dast schema
-}
decoder : List ( BlockId, a ) -> Decoder (StructuredText a)
decoder blocks =
    Decode.field "schema" (constantStringDecoder "Invalid schema type" "dast")
        |> Decode.andThen (\() -> Decode.field "document" (documentDecoder blocks))
        |> Decode.map StructuredText


{-| Decodes a DatoCMS block with its block ID
-}
blockDecoder : Decoder a -> Decoder ( BlockId, a )
blockDecoder blockContentDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "id" blockIdDecoder)
        blockContentDecoder


blockIdDecoder : Decoder BlockId
blockIdDecoder =
    Decode.map BlockId Decode.string


documentDecoder : List ( BlockId, a ) -> Decoder (List (RootChildNode a))
documentDecoder blocks =
    Decode.field "type" (constantStringDecoder "Invalid root type" "root")
        |> Decode.andThen (\() -> Decode.field "children" (Decode.list (rootChildNodeDecoder blocks)))


rootChildNodeDecoder : List ( BlockId, a ) -> Decoder (RootChildNode a)
rootChildNodeDecoder blocks =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "thematicBreak" ->
                        Decode.map RootThematicBreak thematicBreakDecoder

                    "code" ->
                        Decode.map RootCode codeNodeDecoder

                    "paragraph" ->
                        Decode.map RootParagraph paragraphNodeDecoder

                    "heading" ->
                        Decode.map RootHeading headingNodeDecoder

                    "list" ->
                        Decode.map RootList listNodeDecoder

                    "blockquote" ->
                        Decode.map RootBlockquote blockquoteNodeDecoder

                    "block" ->
                        Decode.map RootBlock (blockNodeDecoder blocks)

                    _ ->
                        Decode.fail ("Unknown or unallowed root node type " ++ typeString)
            )


thematicBreakDecoder : Decoder ThematicBreakNode
thematicBreakDecoder =
    Decode.succeed ThematicBreakNode


codeNodeDecoder : Decoder CodeNode
codeNodeDecoder =
    Decode.map3
        (\code language highlight ->
            CodeNode { code = code, language = language, highlight = highlight }
        )
        (Decode.field "code" Decode.string)
        (Decode.optionalField "language" Decode.string)
        (Decode.optionalField "highlight" (Decode.list Decode.int)
            |> Decode.map (Maybe.withDefault [])
        )


paragraphNodeDecoder : Decoder ParagraphNode
paragraphNodeDecoder =
    Decode.field "children" (Decode.list paragraphChildNodeDecoder)
        |> Decode.map ParagraphNode


paragraphChildNodeDecoder : Decoder ParagraphChildNode
paragraphChildNodeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "span" ->
                        spanNodeDecoder |> Decode.map ParagraphSpan

                    "link" ->
                        linkNodeDecoder |> Decode.map ParagraphLink

                    _ ->
                        Decode.fail ("Unknown paragraph child node type " ++ typeString)
            )


spanNodeDecoder : Decoder SpanNode
spanNodeDecoder =
    Decode.map2 (\value marks -> SpanNode { value = value, marks = marks })
        (Decode.field "value" Decode.string)
        (Decode.optionalField "marks" (Decode.list markDecoder)
            |> Decode.map (Maybe.withDefault [])
        )


linkNodeDecoder : Decoder LinkNode
linkNodeDecoder =
    Decode.map3 (\url meta children -> LinkNode { url = url, meta = meta } children)
        (Decode.field "url" Decode.string)
        (Decode.optionalField "meta" (Decode.list linkMetaDecoder)
            |> Decode.map (Maybe.withDefault [])
        )
        (Decode.field "children" (Decode.list linkNodeChildDecoder))


linkMetaDecoder : Decoder ( String, String )
linkMetaDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "id" Decode.string)
        (Decode.field "value" Decode.string)


linkNodeChildDecoder : Decoder LinkChildNode
linkNodeChildDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "span" ->
                        spanNodeDecoder |> Decode.map LinkSpan

                    _ ->
                        Decode.fail ("Unknown link child node type " ++ typeString)
            )


markDecoder : Decoder Mark
markDecoder =
    Decode.string
        |> Decode.andThen
            (\markType ->
                case markType of
                    "strong" ->
                        Decode.succeed Strong

                    "code" ->
                        Decode.succeed Code

                    "emphasis" ->
                        Decode.succeed Emphasis

                    "underline" ->
                        Decode.succeed Underline

                    "strikethrough" ->
                        Decode.succeed Strikethrough

                    "highlight" ->
                        Decode.succeed Highlight

                    _ ->
                        Decode.fail ("Invalid value for mark: " ++ markType)
            )


headingNodeDecoder : Decoder HeadingNode
headingNodeDecoder =
    Decode.map2 (\level children -> HeadingNode { level = level } children)
        (Decode.field "level" headingLevelDecoder)
        (Decode.field "children" (Decode.list headingChildNodeDecoder))


headingChildNodeDecoder : Decoder HeadingChildNode
headingChildNodeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "span" ->
                        spanNodeDecoder |> Decode.map HeadingSpan

                    "link" ->
                        linkNodeDecoder |> Decode.map HeadingLink

                    _ ->
                        Decode.fail ("Unknown heading child node type " ++ typeString)
            )


headingLevelDecoder : Decoder HeadingLevel
headingLevelDecoder =
    Decode.int
        |> Decode.andThen
            (\level ->
                case level of
                    1 ->
                        Decode.succeed H1

                    2 ->
                        Decode.succeed H2

                    3 ->
                        Decode.succeed H3

                    4 ->
                        Decode.succeed H4

                    5 ->
                        Decode.succeed H5

                    6 ->
                        Decode.succeed H6

                    _ ->
                        Decode.fail ("Invalid heading level value: " ++ String.fromInt level)
            )


listNodeDecoder : Decoder ListNode
listNodeDecoder =
    Decode.map2 (\style children -> ListNode { style = style } children)
        (Decode.field "style" listStyleDecoder)
        (Decode.field "children" (Decode.list listChildNodeDecoder))


listStyleDecoder : Decoder ListStyle
listStyleDecoder =
    Decode.string
        |> Decode.andThen
            (\listStyle ->
                case listStyle of
                    "bulleted" ->
                        Decode.succeed Bulleted

                    "numbered" ->
                        Decode.succeed Numbered

                    _ ->
                        Decode.fail ("Invalid list style value: " ++ listStyle)
            )


listChildNodeDecoder : Decoder ListChildNode
listChildNodeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "listItem" ->
                        listItemNodeDecoder |> Decode.map ListListItem

                    _ ->
                        Decode.fail ("Unknown list child node type " ++ typeString)
            )


listItemNodeDecoder : Decoder ListItemNode
listItemNodeDecoder =
    Decode.map ListItemNode
        (Decode.field "children" (Decode.list listItemChildNodeDecoder))


listItemChildNodeDecoder : Decoder ListItemChildNode
listItemChildNodeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "paragraph" ->
                        paragraphNodeDecoder |> Decode.map ListItemParagraph

                    "list" ->
                        listNodeDecoder |> Decode.map ListItemList

                    _ ->
                        Decode.fail ("Unknown list item child node type " ++ typeString)
            )


blockquoteNodeDecoder : Decoder BlockquoteNode
blockquoteNodeDecoder =
    Decode.map2 (\attribution children -> BlockquoteNode { attribution = attribution } children)
        (Decode.optionalField "attribution" Decode.string)
        (Decode.field "children" (Decode.list blockquoteNodeChildDecoder))


blockquoteNodeChildDecoder : Decoder BlockquoteChildNode
blockquoteNodeChildDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "paragraph" ->
                        paragraphNodeDecoder |> Decode.map BlockquoteParagraph

                    _ ->
                        Decode.fail ("Unknown blockquote child node type " ++ typeString)
            )


blockNodeDecoder : List ( BlockId, a ) -> Decoder (BlockNode a)
blockNodeDecoder blocks =
    Decode.field "item" blockIdDecoder
        |> Decode.andThen
            (\blockIdToFind ->
                List.find (\( blockId, _ ) -> blockId == blockIdToFind) blocks
                    |> Maybe.map (\( blockId, blockContent ) -> BlockNode blockId blockContent |> Decode.succeed)
                    |> Maybe.withDefault (Decode.fail ("Unable to find a matching block with ID " ++ blockIdToString blockIdToFind))
            )


blockIdToString : BlockId -> String
blockIdToString (BlockId blockId) =
    blockId


{-| Parse a StructuredText into elm/html nodes
-}
toHtml : StructuredText a -> Html msg
toHtml structuredText =
    text ""


constantStringDecoder : String -> String -> Decoder ()
constantStringDecoder failureReason expectedValue =
    Decode.string
        |> Decode.andThen
            (\stringValue ->
                if stringValue == expectedValue then
                    Decode.succeed ()

                else
                    Decode.fail failureReason
            )
