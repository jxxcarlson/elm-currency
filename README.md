
# A Currency Model

A new model for currency with named currencies that 
can be either fiat or complementary, and which
have issue and expiration times.  The latter may be 
finite or infinite.

```elm
type Money =
      Money { amount : Cents
            , currency : Currency
            , issuedAt : BankTime
            , expiresAt : Expiration
            }
```            
One creates money this way:

```elm
    greenBucks : Currency
    greenBucks = createCompCurrency "Greenbucks"

    m : Money
    m = createFinite greenBucks 0 365 100.21

    toString m
    --> "100.21 Greenbucks (C) 0:365"
```


And and acccount like this:

```elm
    createAccountWithCurrency greenBucks [m] 
       |> value (bankTime 0) |> valueToString
    --> "100.21 Greenbucks (C)"

    createAccountWithCurrency greenBucks [m] 
       |> value (bankTime -1) |> valueToString
    --> "0 Greenbucks (C)"

    createAccountWithCurrency greenBucks [m] 
       |> value (bankTime 366) |> valueToString
    --> "0 Greenbucks (C)" 
```

See the documentation for module `Currency` for more information, e,g,, 
how credit or debit an account.

**Note:** This is work in progress.