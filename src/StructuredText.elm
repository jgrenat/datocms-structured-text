module StructuredText exposing (StructuredText)

{-|

@docs StructuredText

-}

import Internal.Types exposing (Document)


{-| Represents a DatoCMS DAST document used in Structured Text fields. The `a` parameter represents the type of the custom blocks used inside the document.
-}
type alias StructuredText a =
    Document a
