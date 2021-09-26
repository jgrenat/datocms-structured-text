module StructuredText exposing (StructuredText, ItemId)

{-|

@docs StructuredText, ItemId

-}

import Internal.Types exposing (Document, DocumentItemId)


{-| Represents a DatoCMS DAST document used in Structured Text fields. The `a` parameter represents the type of the custom blocks used inside the document.
-}
type alias StructuredText a =
    Document a


{-| Represents an item ID.
-}
type alias ItemId =
    DocumentItemId
