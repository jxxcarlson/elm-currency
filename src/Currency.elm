module Currency exposing(..)

{-| The currency module models currency with an identity
and an expiration.

-}

import List.Extra
import String.Interpolate exposing(interpolate)


{-| Currency is the fundamental type of this module.
A Currency value has an amount, a type (Fiat or Complementary).
a time at which it was issued, and an expiration period,
which is either Infinite or Finite BankTime

    c1 = {amount = Cents 123, currency = Complementary Cambiatus, issueTime = 0, expiration = Finite 100 }

-}
type Money =
    Money { amount : Cents
    , currency : Currency
    , issuedAt : BankTime
    , expiresAt : Expiration
    }

amount : Money -> Cents
amount (Money m) =
    m.amount

currency : Money -> Currency
currency (Money m) = m.currency


issuedAt : Money -> BankTime
issuedAt (Money m) = m.issuedAt

expiresAt : Money -> Expiration
expiresAt (Money m) = m.expiresAt


type Value = Value Currency Cents

type Cents = Cents Int

type BankTime = BankTime Int

type Expiration = Infinite | Finite BankTime


{-| CurrencyType is either Fiat (like dollars), or Complementary
-}
type Currency
    = Fiat CurrencyName
    | Complementary CurrencyName


type CurrencyName =  Cambiatus | Real | Dollar


{-| An account is a list of Money values for  given currency.
Such a structures needed, since different values
may have different expiration periods, etc.

It is assumed that all Money values are denominated
in the same Currency.  This restriction is enforced
by the functions which operate on accounts.
-}
type  Account =
      Account  { currency: Currency, transactions : List Money }


-- OPERATIONS AND FUNCTIONS --

{-|

    isValid (BankTime -1)  <| Money  {amount = Cents 123, currency = Complementary Cambiatus, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }
    --> False

    isValid (BankTime 0) <| Money {amount = Cents 123, currency = Complementary Cambiatus, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }
    --> True

    isValid (BankTime 1) <| Money {amount = Cents 123, currency = Complementary Cambiatus, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }
    --> True

    isValid (BankTime 100) <| Money {amount = Cents 123, currency = Complementary Cambiatus, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }
    --> True

    isValid (BankTime 101) <| Money {amount = Cents 123, currency = Complementary Cambiatus, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }
    --> False

    isValid (BankTime 101) <| Money {amount = Cents 123, currency = Complementary Cambiatus, issuedAt = BankTime 0, expiresAt = Infinite }
    --> True


-}
isValid : BankTime -> Money -> Bool
isValid (BankTime currentTime) (Money m) =
    case m.issuedAt of
        (BankTime issueTime) ->
            if issueTime > currentTime then
              False
            else
              case m.expiresAt of
                  Infinite -> True
                  Finite (BankTime expirationTime) ->
                      expirationTime  >=  currentTime


{-|
   c1 : Money
   c1 =  Money {amount = Cents 123, currency = Complementary Cambiatus, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

   emptyAcct : Account
   emptyAcct = Account  { currency = Complementary Cambiatus, transactions = []}

   acct : Account
   acct = Account  { currency = Complementary Cambiatus, transactions = [c1]})

   ensureValid (BankTime 10) acct
   --> acct

   ensureValid (BankTime 101 acct
    --> emptyAcct

-}
ensureValid : BankTime -> Account -> Account
ensureValid bankTime (Account acct) =
    Account {currency = acct.currency, transactions = List.filter (isValid bankTime) acct.transactions}


{-|
    c1 : Money
    c1 =  Money {amount = Cents 123, currency = Complementary Cambiatus, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    acct : Account
    acct = Account  { currency = Complementary Cambiatus, transactions = [c1]}

    acct2 : Account
    acct2 = Account  { currency = Complementary Cambiatus, transactions = [c1,c1]}

    value (BankTime 10) acct
    --> Value (Complementary Cambiatus) (Cents 123)

    value (BankTime 101) acct
    --> Value (Complementary Cambiatus) (Cents 0)

    value (BankTime -1) acct
    --> Value (Complementary Cambiatus) (Cents 0)

    value (BankTime 10) acct2
    --> Value (Complementary Cambiatus) (Cents 246)

-}
value : BankTime -> Account -> Value
value  bankTime ((Account acct) as account)=
  account
    |> ensureValid bankTime
    |> valueInCents
    |> (\v -> Value acct.currency v)



{-|

    c1 : Money
    c1 =  Money {amount = Cents 123, currency = Complementary Cambiatus, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    emptyAcct : Account
    emptyAcct = Account{ currency = Complementary Cambiatus, transactions = []}

    acct : Account
    acct = Account  { currency = Complementary Cambiatus, transactions = [c1]}

    acct2 : Account
    acct2 = Account  { currency = Complementary Cambiatus, transactions = [c1, c1]}

    valueInCents emptyAcct
    --> (Cents 0)

    valueInCents acct
    --> (Cents 123)

    valueInCents acct2
    --> (Cents 246)

-}
valueInCents : Account -> Cents
valueInCents (Account acct) =
  case acct.transactions of
      [] -> (Cents 0)
      transactions ->
         transactions
            |> List.map amount
            |> List.map (\(Cents k) -> k)
            |> List.sum
            |> (\s -> Cents s)


-- CONVERSIONS --


{-|

    stringFromCents (Cents 123)
    --> "1.23"

-}
stringFromCents : Cents -> String
stringFromCents cents =
    floatFromCents cents |> String.fromFloat


{-|

    floatFromCents (Cents 123) |> String.fromFloat
    --> "1.23"

    floatFromCents (Cents -123) |> String.fromFloat
        --> "-1.23"

-}
floatFromCents : Cents -> Float
floatFromCents (Cents k) =
    (toFloat k) / 100.0


stringFromExpiration : Expiration -> String
stringFromExpiration e =
  case e of
      Finite t -> stringFromBankTime t
      Infinite ->  "Infinite"



stringFromBankTime : BankTime -> String
stringFromBankTime (BankTime k) =
    String.fromInt k

{-|

    stringFromMoney <| Money  {amount = Cents 123, currency = Complementary Cambiatus, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }
    --> "1.23 Cambiatus (C) 0:100"

    stringFromMoney <| Money {amount = Cents 123, currency = Complementary Cambiatus, issuedAt = BankTime 0, expiresAt = Infinite }
    --> "1.23 Cambiatus (C) 0:Infinite"



-}
stringFromMoney : Money -> String
stringFromMoney (Money m) =
  interpolate "{0} {1} {2}:{3}" [stringFromCents m.amount
    , stringFromCurrency m.currency
    , stringFromBankTime m.issuedAt
    , stringFromExpiration m.expiresAt ]


stringFromCurrency : Currency -> String
stringFromCurrency c =
    case c of
        Fiat cn -> interpolate "{0} ({1})" [stringFromCurrencyName cn, "F"]
        Complementary cn -> interpolate "{0} ({1})" [stringFromCurrencyName cn, "C"]

{-|

    stringFromCurrency (Complementary Cambiatus)
    --> "Cambiatus (C)"

-}
stringFromCurrencyName : CurrencyName -> String
stringFromCurrencyName cn =
    case cn of
        Cambiatus -> "Cambiatus"
        Real -> "Real"
        Dollar -> "Dollar"
