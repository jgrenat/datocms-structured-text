module StructuredText.Html exposing (render)

import Html exposing (Html, text)
import StructuredText.Types exposing (StructuredText)


{-| Render a StructuredText document as elm/html nodes
-}
render : StructuredText a -> List (Html msg)
render structuredText =
    Debug.todo "To do"
