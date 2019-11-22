module Money exposing(
     Money
   , Value
   , Cents
   , BankTime
   , Expiration
   , Currency
   , Account
   , toString
   , bankTime
   , createValue
   , credit
   , debit
   , emptyAccount
   , mergeAccounts
   , createInfinite
   , createFinite
   , createCompCurrency
   , createFiatCurrency
   , createAccountWithCurrency
   , value
   , valueToString
   )

{-| A model for currency with an identity
     and an expiration.

## Money, Account, and Value

@docs Money, Account, Value, bankTime, toString, valueToString

## Component of Money

@docs Cents, BankTime, Expiration, Currency

## Creating things

@docs createCompCurrency, createFiatCurrency, createFinite, createInfinite, createAccountWithCurrency, emptyAccount

## Operations on Accounts

@docs value, credit, debit





-}

import Internal.Money as Internal exposing(
  Money
  , Value
  , Cents
  , BankTime
  , Expiration
  , Currency
  , CurrencyType(..)
  , value
  , credit
  , debit
  , bankTime
  , createInfinite
  , createFinite
  , valueToString
  , createCompCurrency
  , createFiatCurrency
  , createAccountWithCurrency
  , emptyAccount
  , greenBucks)

{-| Money is the fundamental type of this module.
A value of type Money has an amount, a type (Fiat or Complementary).
a time at which it was issued, and an expiration time,
which is either Infinite or Finite BankTime

    greenBucks : Currency
    greenBucks = createCompCurrency "Greenbucks"

    m : Money
    m = createFinite greenBucks 0 365 100.21

    toString m
    --> "100.21 Greenbucks (C) 0:365"

-}
type alias Money = Internal.Money


{-| An account is a list of Money values for  given currency.
Such a structures needed, since different values
may have different expiration periods, etc.

It is assumed that all Money values are denominated
in the same Currency.  This restriction is enforced
by the functions which operate on accounts.

-}
type  alias Account = Internal.Account

mergeAccounts : List Account -> Maybe Account
mergeAccounts = Internal.mergeAccounts

{-| An account at a given time has a Value

    greenBucks : Currency
    greenBucks = createCompCurrency "Greenbucks"

    m1 : Money
    m1 = createFinite greenBucks 0 365 100.00

    m2 : Money
    m2 = createInFinite greenBucks 0 50.00

    acct : Account
    acct = createAccountWithCurrency greenBucks [m1, m2]
    value (bankTime 0) acct |> valueToString
    --> "150 Greenbucks (C)"


-}
type alias Value = Internal.Value


createValue : Currency -> Float -> Value
createValue = Internal.createValue

{-| Return a string representation of a Value

-}
valueToString : Value -> String
valueToString = Internal.valueToString


{-| Create a BankTime value from an Integer

-}
bankTime : Int -> BankTime
bankTime = Internal.bankTime

{-|  We denominate money in integer Cents so as
to avoid round-off error.
-}
type alias Cents = Internal.Cents


{-| BankTime t is an abstract integer time which
can be seconds, days, weeks, whatever
-}
type alias BankTime = Internal.BankTime


{-| A currency may be finite or infinite,
that is, expiring or non-expiring.

-}
type alias Expiration = Internal.Expiration


{-| Currency can be fiat or complementary
-}
type alias Currency = Internal.Currency

type alias CurrencyType = Internal.CurrencyType

{-|

    greenBucks : Currency
    greenBucks = createCompCurrency "Greenbucks"

    m1 : Money
    m1 = createFinite greenBucks 0 365 100.21

    m2 : Money
    m2 = createFinite greenBucks 0 365 3.17


    createAccountWithCurrency greenBucks []
    --> emptyAccount greenBucks

    acct : Account
    acct = createAccountWithCurrency greenBucks [m1]
    --> Account { currency = greenBucks, transactions = [m2] }


-}
createAccountWithCurrency : Currency -> List Money -> Account
createAccountWithCurrency = Internal.createAccountWithCurrency

{-| Create an account ofr the given currency with empty transaction list -}
emptyAccount : Currency -> Account
emptyAccount = Internal.emptyAccount

{-| Create a complementary currency with given name -}
createCompCurrency :  String  -> Currency
createCompCurrency = Internal.createCompCurrency

{-| Create a fiat currency with given name -}
createFiatCurrency :  String  -> Currency
createFiatCurrency = Internal.createFiatCurrency

{-| Create money with an expiration date

  greenBucks = createCompCurrency "GreenBucks"
  m1 = createFinite greenBucks 0 365 100.01

-}
createFinite : Currency ->  Int -> Int -> Float -> Money
createFinite = Internal.createFinite

{-| Create money which does not expire -}
createInfinite : Currency ->  Int -> Float -> Money
createInfinite = Internal.createInfinite


{-|
A string representation of Money
-}
toString : Money -> String
toString = Internal.stringFromMoney


{-|

Return the value of an account at a given time.

-}
value : BankTime -> Account -> Value
value  = Internal.value


{-|

Credit an account with a given amount of money at a given time, simplify the result.


--}
credit : BankTime -> Money -> Account -> Account
credit = Internal.credit

{-|


Debit an account with a given amount of money at a given time, simplify the result.



-}
debit : BankTime -> Money -> Account -> Account
debit = Internal.debit

