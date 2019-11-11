module VerifyExamples.Currency.FloatFromCents1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Currency exposing (..)







spec1 : Test.Test
spec1 =
    Test.test "#floatFromCents: \n\n    floatFromCents (Cents 123) |> String.fromFloat\n    --> \"1.23\"" <|
        \() ->
            Expect.equal
                (
                floatFromCents (Cents 123) |> String.fromFloat
                )
                (
                "1.23"
                )