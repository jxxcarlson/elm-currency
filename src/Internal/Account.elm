module Internal.Account exposing (..)


import Internal.Money as Money exposing(BankTime, Currency, Money, Cents(..), Value(..))
import Internal.Utility as Utility
import Maybe.Extra

{-| An account is a list of Money values for  given currency.
Such a structures needed, since different values
may have different expiration periods, etc.

It is assumed that all Money values are denominated
in the same Currency.  This restriction is enforced
by the functions which operate on accounts.
-}
type  Account =
      Account  { currency: Currency, transactions : List Money }

mergeAccounts : List Account -> Maybe Account
mergeAccounts list =
    case (Maybe.map getAccountCurrency (List.head list)) of
        Just currency_ ->
            Just <| Account { currency = currency_, transactions = List.concat (List.map getTransactions list) }
        Nothing -> Nothing


getAccountCurrency : Account -> Currency
getAccountCurrency (Account data) =
    data.currency

getTransactions : Account -> List Money
getTransactions (Account data) =
    data.transactions


{-|

    m1 : Money
    m1 = createFinite greenBucks 0 365 100.21

    createAccount [m1, m1]
    --> Just <| Account { currency = greenBucks, transactions = [m1, m1] }

-}
createAccount : List Money -> Maybe Account
createAccount list =
   case List.head list of
       Nothing -> Nothing
       Just m ->
           case Money.sameCurrency list of
               False -> Nothing
               True -> Just <| Account {currency = Money.currency m, transactions = list}




{-|

    m1 : Money
    m1 = createFinite greenBucks 0 365 100.21

    createAccountWithCurrency greenBucks []
    --> emptyAccount greenBucks

    createAccountWithCurrency greenBucks [m1]
    --> Account { currency = greenBucks, transactions = [m1] }

    createAccountWithCurrency greenBucks [m1] |> value (bankTime 0) |> valueToString
    --> "100.21 Greenbucks (C)"

    createAccountWithCurrency greenBucks [m1] |> value (bankTime -1) |> valueToString
    --> "0 Greenbucks (C)"

    createAccountWithCurrency greenBucks [m1] |> value (bankTime 366) |> valueToString
    --> "0 Greenbucks (C)"
-}
createAccountWithCurrency : Currency -> List Money -> Account
createAccountWithCurrency currency_ list =
    let
       currencies = List.map Money.currency list
     in
     case Utility.andOfList (List.map (\c -> c == currency_) currencies) of
         True -> Account {currency = currency_, transactions = list}
         False -> emptyAccount currency_



emptyAccount : Currency -> Account
emptyAccount currency_ =
    Account {currency = currency_, transactions = []}


{-|
    c1 : Money
    c1 =  Money {amount = Cents 123, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    acct : Account
    acct = Account  { currency = greenBucks, transactions = [c1]}

    acct2 : Account
    acct2 = Account  { currency = greenBucks, transactions = [c1,c1]}

    value (BankTime 10) acct
    --> Value (greenBucks) (Cents 123)

    value (BankTime 101) acct
    --> Value (greenBucks) (Cents 0)

    value (BankTime -1) acct
    --> Value (greenBucks) (Cents 0)

    value (BankTime 10) acct2
    --> Value (greenBucks) (Cents 246)

-}
value : BankTime -> Account -> Value
value  bankTime_ ((Account acct) as account)=
  account
    |> ensureValid bankTime_
    |> valueInCents
    |> (\v -> Value acct.currency v)


{-|

    c1 : Money
    c1 =  Money {amount = Cents 100, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    c2 : Money
    c2 =  Money {amount = Cents 200, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    acct : Account
    acct = Account {currency = greenBucks, transactions = [c1]}

    acct2 : Account
    acct2 = Account {currency = greenBucks, transactions = [c2]}

    credit (BankTime 0) c1 acct
    --> acct2
-}
credit : BankTime -> Money -> Account -> Account
credit bt m (Account acct) =
    simplify bt (Account { acct | transactions = m::acct.transactions})

{-|

    c1 : Money
    c1 =  Money {amount = Cents 100, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    c2 : Money
    c2 =  Money {amount = Cents 0, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    acct : Account
    acct = Account {currency = greenBucks, transactions = [c1]}

    acct2 : Account
    acct2 = Account {currency = greenBucks, transactions = [c2]}

    debit (BankTime 0) c1 acct
    --> acct2
-}
debit : BankTime -> Money -> Account -> Account
debit bt m (Account acct) =
   simplify bt (Account { acct | transactions = (Money.negate m)::acct.transactions})



{-|

    c1 : Money
    c1 =  Money {amount = Cents 100, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    c2 : Money
    c2 =  Money {amount = Cents 200, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    c3 : Money
    c3 =  Money {amount = Cents 300, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }


    c4 : Money
    c4 =  Money {amount = Cents 10, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Infinite }

    acct : Account
    acct = Account {currency = greenBucks, transactions = [c1,c2]}

    simplify (BankTime 0) acct
    --> Account {currency = greenBucks, transactions = [c3]}

    simplify (BankTime 0) (Account {currency = greenBucks, transactions = [c1,c2, c4]})
    --> Account {currency = greenBucks, transactions = [c3, c4]}
-}
simplify : BankTime -> Account -> Account
simplify bt ((Account acct) as account) =
   let
       (Account acct2) = ensureValid bt account
       groups = Money.group acct2.transactions
       transactions = List.map Money.consolidate groups
         |> Maybe.Extra.values
   in
     Account {currency = acct.currency, transactions = transactions}



{-|
   c1 : Money
   c1 =  Money {amount = Cents 123, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

   emptyAcct : Account
   emptyAcct = Account  { currency = greenBucks, transactions = []}

   acct : Account
   acct = Account  { currency = greenBucks, transactions = [c1]})

   ensureValid (BankTime 10) acct
   --> acct

   ensureValid (BankTime 101 acct
    --> emptyAcct

-}
ensureValid : BankTime -> Account -> Account
ensureValid bankTime_ (Account acct) =
    Account {currency = acct.currency, transactions = List.filter (Money.isValid bankTime_) acct.transactions}





{-|

    c1 : Money
    c1 =  Money {amount = Cents 123, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    emptyAcct : Account
    emptyAcct = Account{ currency = greenBucks, transactions = []}

    acct : Account
    acct = Account  { currency = greenBucks, transactions = [c1]}

    acct2 : Account
    acct2 = Account  { currency = greenBucks, transactions = [c1, c1]}

    valueInCents emptyAcct
    --> (Cents 0)

    valueInCents acct
    --> (Cents 123)

    valueInCents acct2
    --> (Cents 246)

-}
valueInCents : Account -> Cents
valueInCents (Account acct) =
    valueInCents_ acct.transactions


valueInCents_ : List Money -> Cents
valueInCents_ list  =
  case list of
      [] -> (Cents 0)
      _ ->
         list
            |> List.map Money.amount
            |> List.map (\(Cents k) -> k)
            |> List.sum
            |> (\s -> Cents s)

