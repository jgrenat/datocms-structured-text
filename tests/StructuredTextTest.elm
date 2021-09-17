module StructuredTextTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (Error(..), errorToString)
import Json.Encode as Encode
import StructuredText exposing (decoder)
import Test exposing (..)
import Types exposing (Node(..), StructuredText(..))


suite : Test
suite =
    describe "StructuredText"
        [ describe "decoder"
            [ test "should decode an empty document" <|
                \_ ->
                    let
                        document =
                            """{ "schema": "dast", "document": { "type": "root", "children": [] } }"""

                        parsedDocument =
                            Decode.decodeString decoder document
                    in
                    Expect.equal parsedDocument (Ok (StructuredText []))
            , test "should fail with an invalid schema value" <|
                \_ ->
                    let
                        document =
                            """{ "schema": "dats", "document": { "type": "root", "children": [] } }"""

                        parsedDocument =
                            Decode.decodeString decoder document
                    in
                    case parsedDocument of
                        Ok _ ->
                            Expect.fail "This should fail because of the invalid schema type 'dats'"

                        Err (Field "schema" (Failure reason _)) ->
                            Expect.equal reason "Invalid schema"

                        Err failure ->
                            Expect.fail ("Wrong failure reason: " ++ errorToString failure)
            , test "should fail with an invalid root type" <|
                \_ ->
                    let
                        document =
                            """{ "schema": "dast", "document": { "type": "roto", "children": [] } }"""

                        parsedDocument =
                            Decode.decodeString decoder document
                    in
                    case parsedDocument of
                        Ok _ ->
                            Expect.fail "This should fail because of the invalid root type 'roto'"

                        Err (Field "document" (Failure reason _)) ->
                            Expect.equal reason "Invalid root type"

                        Err failure ->
                            Expect.fail ("Wrong failure reason: " ++ errorToString failure)
            , test "should decode a single span child" <|
                \_ ->
                    let
                        document =
                            """{ "schema": "dast", "document": { "type": "root", "children": [
                            {
                              "type": "span",
                              "value": "Some text content"
                            }
                            ] } }"""

                        parsedDocument =
                            Decode.decodeString decoder document

                        expected =
                            StructuredText [ Span "Some text content" ]
                    in
                    Expect.equal parsedDocument (Ok expected)
            ]
        ]
