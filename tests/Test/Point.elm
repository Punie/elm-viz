module Test.Point exposing (suite)

import Expect
import Json.Decode
import Point
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Point"
        [ test "decode mock1" <|
            \_ ->
                Point.mock1
                    |> Json.Decode.decodeString Point.decoder
                    |> Expect.ok
        , test "decode mock2" <|
            \_ ->
                Point.mock2
                    |> Json.Decode.decodeString Point.decoder
                    |> Expect.ok
        ]
