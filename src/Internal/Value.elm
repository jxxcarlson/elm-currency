module Internal.Value exposing (..)

import Internal.Types exposing(Value(..), Cents(..), Currency(..))
import Internal.Cents as Cents

map : (Cents -> Cents) -> Value -> Value
map f (Value curr cents) =
    (Value curr (f cents))

map2 : (Cents -> Cents -> Cents ) -> Value -> Value -> Maybe Value
map2 f (Value curr1 cents1)  (Value curr2 cents2)=
    case curr1 == curr2 of
        True ->  Just (Value curr1 (f cents1 cents2))
        False -> Nothing


{-|
    import Internal.Types exposing(Value(..), Cents(..))

    v1 : Value
    v1 = Value usDollars (Cents 200)

    v2 : Value
    v2 = Value usDollars (Cents 100)

    v3 : Value
    v3 = Value greenBucks (Cents 100)

    add v1 v2
    -- Just <| Value (Cents 300)

    add v1 v3
    --> Nothing


-}
add : Value -> Value -> Maybe Value
add a b =
    map2 Cents.add a b

{-|
    import Internal.Types exposing(Value(..), Cents(..))

    v1 : Value
    v1 = Value usDollars (Cents 200)

    v2 : Value
    v2 = Value usDollars (Cents 100)

    v3 : Value
    v3 = Value greenBucks (Cents 100)

    subtract v1 v2
    -- Just <| Value (Cents 100)

    subtract v1 v3
    --> Nothing


-}
subtract : Value -> Value -> Maybe Value
subtract a b =
    map2 Cents.subtract a b



--gtValue : Value -> Value -> Maybe Bool
--gtValue a b =


create : Currency -> Float -> Value
create currency_ amount_ =
    Value currency_ (Cents (round (amount_/100.0)))


