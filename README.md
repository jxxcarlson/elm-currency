
# A Currency Model

A new model for currency with named currencies that 
can be either fiat or complementary, and which
have issue and expiration times.  The latter may be 
finite or infinite.

```
type Money =
      Money { amount : Cents
            , currency : Currency
            , issuedAt : BankTime
            , expiresAt : Expiration
            }
```            

The `issuedAt` and `expiresAt` are defined via the types

```
type BankTime = BankTime Int

type Expiration = Infinite | Finite BankTime
```

so that we may have both expiring and non-expiring currency.  
All values are now wrapped in a constructor, e.g., 
`BankTime Int` instead of just `Int`.  
This makes it impossible to do nonsensical operations.  
One can think of it as every quantity having a unit of
 measure, like in physics.  
 Thus the value of money is denominated in `Cents Int`.  
 Therefore, using the given API you can’t even construct a 
 function that  adds money to time, for example, just as in physics 
 you can’t add energy and momentum, since they have d
 ifferent units/dimensions.    
 I decided to use `Cents Int` rather than `Float` 
 because this way there are no round-off errors.  
 In this setup there is a notion of

```
type Value = Value Currency Cents
```

A value like Value (Complementary Cambiatus) (Cents 123)
would be displayed as

     1.23 Cambiatus (C)

whereas Value (Fiat Real) (Cents 123) would be displayed as

    1.23 Real (F)

An Account is

```
type  Account =
      Account  { currency: Currency, transactions : List Money }
```
Transactions all have to be in the same currency,
 a restriction which is enforced by type system 
 and the functions available in the API 
 for manipulating these things.

Finally (for tonight) there is the important function

```
value : BankTime -> Account -> Value
```


which computes the value of an account at a  given time, 
taking into consideration issue and expiration dates.  

**Note.** 

We are using `elm-verify-examples` to both document and test the code.