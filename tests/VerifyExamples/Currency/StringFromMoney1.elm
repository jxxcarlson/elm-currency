module VerifyExamples.Currency.StringFromMoney1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Currency exposing (..)







spec1 : Test.Test
spec1 =
    Test.test "#stringFromMoney: \n\n    stringFromMoney {amount = Cents 123, currency = Complementary Cambiatus, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }\n    --> \"1.23 Cambiatus (C) 0:100\"" <|
        \() ->
            Expect.equal
                (
                stringFromMoney {amount = Cents 123, currency = Complementary Cambiatus, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }
                )
                (
                "1.23 Cambiatus (C) 0:100"
                )