module Account exposing (..)

import Internal.Money exposing(Money, Currency, Value, BankTime)

import Internal.Account exposing(Account)



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
createAccountWithCurrency = Internal.Account.createAccountWithCurrency

{-| Create an account ofr the given currency with empty transaction list -}
emptyAccount : Currency -> Account
emptyAccount = Internal.Account.emptyAccount

{-|

Return the value of an account at a given time.

-}
value : BankTime -> Account -> Value
value  = Internal.Account.value


{-|

Credit an account with a given amount of money at a given time, simplify the result.


--}
credit : BankTime -> Money -> Account -> Account
credit = Internal.Account.credit

{-|


Debit an account with a given amount of money at a given time, simplify the result.



-}
debit : BankTime -> Money -> Account -> Account
debit = Internal.Account.debit


{-| An account is a list of Money values for  given currency.
Such a structures needed, since different values
may have different expiration periods, etc.

It is assumed that all Money values are denominated
in the same Currency.  This restriction is enforced
by the functions which operate on accounts.

-}


mergeAccounts : List Account -> Maybe Account
mergeAccounts = Internal.Account.mergeAccounts


