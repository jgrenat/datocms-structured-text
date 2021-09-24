module HtmlTest exposing (..)

import Expect exposing (Expectation)
import StructuredText.Html
import StructuredText.Types exposing (StructuredText(..))
import Test exposing (..)


suite : Test
suite =
    describe "The StructuredText.Html module"
        [ describe "decoder"
            [ test "should render an empty document" <|
                \_ ->
                    let
                        input =
                            StructuredText []

                        result =
                            StructuredText.Html.render input
                    in
                    Expect.equal [] result
            ]
        ]
