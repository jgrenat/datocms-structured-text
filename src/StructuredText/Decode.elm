module StructuredText.Decode exposing (decoder, itemDecoder, toHtml)

import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import List.Extra as List
import StructuredText.Types exposing (BlockNode(..), BlockquoteChildNode(..), BlockquoteNode(..), CodeNode(..), HeadingChildNode(..), HeadingLevel(..), HeadingNode(..), InlineItemNode(..), ItemId(..), ItemLinkChildNode(..), ItemLinkNode(..), LinkChildNode(..), LinkNode(..), ListChildNode(..), ListItemChildNode(..), ListItemNode(..), ListNode(..), ListStyle(..), Mark(..), ParagraphChildNode(..), ParagraphNode(..), RootChildNode(..), SpanNode(..), StructuredText(..), ThematicBreakNode(..))


{-| Decodes a DatoCMS Dast schema
-}
decoder : List ( ItemId, a ) -> Decoder (StructuredText a)
decoder items =
    Decode.field "schema" (constantStringDecoder "Invalid schema type" "dast")
        |> Decode.andThen (\() -> Decode.field "document" (documentDecoder items))
        |> Decode.map StructuredText


{-| Decodes a DatoCMS item with its item ID
-}
itemDecoder : Decoder a -> Decoder ( ItemId, a )
itemDecoder itemContentDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "id" itemIdDecoder)
        itemContentDecoder


itemIdDecoder : Decoder ItemId
itemIdDecoder =
    Decode.map ItemId Decode.string


documentDecoder : List ( ItemId, a ) -> Decoder (List (RootChildNode a))
documentDecoder items =
    Decode.field "type" (constantStringDecoder "Invalid root type" "root")
        |> Decode.andThen (\() -> Decode.field "children" (Decode.list (rootChildNodeDecoder items)))


rootChildNodeDecoder : List ( ItemId, a ) -> Decoder (RootChildNode a)
rootChildNodeDecoder items =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "thematicBreak" ->
                        Decode.map RootThematicBreak thematicBreakDecoder

                    "code" ->
                        Decode.map RootCode codeNodeDecoder

                    "paragraph" ->
                        Decode.map RootParagraph (paragraphNodeDecoder items)

                    "heading" ->
                        Decode.map RootHeading (headingNodeDecoder items)

                    "list" ->
                        Decode.map RootList (listNodeDecoder items)

                    "blockquote" ->
                        Decode.map RootBlockquote (blockquoteNodeDecoder items)

                    "block" ->
                        Decode.map RootBlock (blockNodeDecoder items)

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


paragraphNodeDecoder : List ( ItemId, a ) -> Decoder (ParagraphNode a)
paragraphNodeDecoder items =
    Decode.field "children" (Decode.list (paragraphChildNodeDecoder items))
        |> Decode.map ParagraphNode


paragraphChildNodeDecoder : List ( ItemId, a ) -> Decoder (ParagraphChildNode a)
paragraphChildNodeDecoder items =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "span" ->
                        spanNodeDecoder |> Decode.map ParagraphSpan

                    "link" ->
                        linkNodeDecoder |> Decode.map ParagraphLink

                    "inlineItem" ->
                        inlineItemNodeDecoder items |> Decode.map ParagraphInlineItem

                    "itemLink" ->
                        itemLinkNodeDecoder items |> Decode.map ParagraphItemLink

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


headingNodeDecoder : List ( ItemId, a ) -> Decoder (HeadingNode a)
headingNodeDecoder items =
    Decode.map2 (\level children -> HeadingNode { level = level } children)
        (Decode.field "level" headingLevelDecoder)
        (Decode.field "children" (Decode.list (headingChildNodeDecoder items)))


headingChildNodeDecoder : List ( ItemId, a ) -> Decoder (HeadingChildNode a)
headingChildNodeDecoder items =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "span" ->
                        spanNodeDecoder |> Decode.map HeadingSpan

                    "link" ->
                        linkNodeDecoder |> Decode.map HeadingLink

                    "inlineItem" ->
                        inlineItemNodeDecoder items |> Decode.map HeadingInlineItem

                    "itemLink" ->
                        itemLinkNodeDecoder items |> Decode.map HeadingItemLink

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


listNodeDecoder : List ( ItemId, a ) -> Decoder (ListNode a)
listNodeDecoder items =
    Decode.map2 (\style children -> ListNode { style = style } children)
        (Decode.field "style" listStyleDecoder)
        (Decode.field "children" (Decode.list (listChildNodeDecoder items)))


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


listChildNodeDecoder : List ( ItemId, a ) -> Decoder (ListChildNode a)
listChildNodeDecoder items =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "listItem" ->
                        listItemNodeDecoder items |> Decode.map ListListItem

                    _ ->
                        Decode.fail ("Unknown list child node type " ++ typeString)
            )


listItemNodeDecoder : List ( ItemId, a ) -> Decoder (ListItemNode a)
listItemNodeDecoder items =
    Decode.map ListItemNode
        (Decode.field "children" (Decode.list (listItemChildNodeDecoder items)))


listItemChildNodeDecoder : List ( ItemId, a ) -> Decoder (ListItemChildNode a)
listItemChildNodeDecoder items =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "paragraph" ->
                        paragraphNodeDecoder items |> Decode.map ListItemParagraph

                    "list" ->
                        listNodeDecoder items |> Decode.map ListItemList

                    _ ->
                        Decode.fail ("Unknown list item child node type " ++ typeString)
            )


blockquoteNodeDecoder : List ( ItemId, a ) -> Decoder (BlockquoteNode a)
blockquoteNodeDecoder items =
    Decode.map2 (\attribution children -> BlockquoteNode { attribution = attribution } children)
        (Decode.optionalField "attribution" Decode.string)
        (Decode.field "children" (Decode.list (blockquoteNodeChildDecoder items)))


blockquoteNodeChildDecoder : List ( ItemId, a ) -> Decoder (BlockquoteChildNode a)
blockquoteNodeChildDecoder items =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "paragraph" ->
                        paragraphNodeDecoder items |> Decode.map BlockquoteParagraph

                    _ ->
                        Decode.fail ("Unknown blockquote child node type " ++ typeString)
            )


blockNodeDecoder : List ( ItemId, a ) -> Decoder (BlockNode a)
blockNodeDecoder items =
    Decode.field "item" itemIdDecoder
        |> Decode.andThen
            (\itemIdToFind ->
                List.find (\( itemId, _ ) -> itemId == itemIdToFind) items
                    |> Maybe.map (\( itemId, itemContent ) -> BlockNode { itemId = itemId, itemContent = itemContent } |> Decode.succeed)
                    |> Maybe.withDefault (Decode.fail ("Unable to find a matching item with ID " ++ itemIdToString itemIdToFind))
            )


inlineItemNodeDecoder : List ( ItemId, a ) -> Decoder (InlineItemNode a)
inlineItemNodeDecoder items =
    Decode.field "item" itemIdDecoder
        |> Decode.andThen
            (\itemIdToFind ->
                List.find (\( itemId, _ ) -> itemId == itemIdToFind) items
                    |> Maybe.map (\( itemId, itemContent ) -> InlineItemNode { itemId = itemId, itemContent = itemContent } |> Decode.succeed)
                    |> Maybe.withDefault (Decode.fail ("Unable to find a matching item with ID " ++ itemIdToString itemIdToFind))
            )


itemLinkNodeDecoder : List ( ItemId, a ) -> Decoder (ItemLinkNode a)
itemLinkNodeDecoder items =
    let
        itemLinkNodeItemDecoder : Decoder ( ItemId, a )
        itemLinkNodeItemDecoder =
            Decode.field "item" itemIdDecoder
                |> Decode.andThen
                    (\itemIdToFind ->
                        List.find (\( itemId, _ ) -> itemId == itemIdToFind) items
                            |> Maybe.map Decode.succeed
                            |> Maybe.withDefault (Decode.fail ("Unable to find a matching item with ID " ++ itemIdToString itemIdToFind))
                    )
    in
    Decode.map3 (\( itemId, itemContent ) meta children -> ItemLinkNode { itemId = itemId, itemContent = itemContent, meta = meta } children)
        itemLinkNodeItemDecoder
        (Decode.optionalField "meta" (Decode.list linkMetaDecoder)
            |> Decode.map (Maybe.withDefault [])
        )
        (Decode.field "children" (Decode.list itemLinkNodeChildDecoder))


itemLinkNodeChildDecoder : Decoder ItemLinkChildNode
itemLinkNodeChildDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "span" ->
                        spanNodeDecoder |> Decode.map ItemLinkSpan

                    _ ->
                        Decode.fail ("Unknown or invalid item link child node type " ++ typeString)
            )


itemIdToString : ItemId -> String
itemIdToString (ItemId blockId) =
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
