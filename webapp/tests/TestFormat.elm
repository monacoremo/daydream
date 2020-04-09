module TestFormat exposing (..)

import Expect exposing (Expectation)
import Format
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "The Format module"
        [ describe "formats cents"
            [ test "positive values" <|
                \() ->
                    Expect.equal
                        (Format.cents 12.04)
                        "12,04"
            , test "negative values" <|
                \() ->
                    Expect.equal
                        (Format.cents -12.34)
                        "-12,34"
            , test "rounds values" <|
                \() ->
                    Expect.equal
                        (Format.cents 12.345678)
                        "12,35"
            ]
        ]
