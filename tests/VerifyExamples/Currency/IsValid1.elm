module VerifyExamples.Currency.IsValid1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Currency exposing (..)







spec1 : Test.Test
spec1 =
    Test.test "#isValid: \n\n    isValid (BankTime 101) {amount = Cents 123, currency = Complementary Cambiatus, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }\n    --> False" <|
        \() ->
            Expect.equal
                (
                isValid (BankTime 101) {amount = Cents 123, currency = Complementary Cambiatus, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }
                )
                (
                False
                )