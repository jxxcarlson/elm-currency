
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
    account : Account 
    account = makeAccount [m, m]
    value account 
    --> 0     
```