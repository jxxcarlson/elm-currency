module Value exposing (create, mul, gt, gte)

import Internal.Types exposing(Value, Currency)
import Internal.Value

create : Currency -> Float -> Value
create = Internal.Value.create

mul : Int -> Value -> Value
mul = Internal.Value.mul

gte : Value -> Value -> Maybe Bool
gte  = Internal.Value.gte

gt : Value -> Value -> Maybe Bool
gt  = Internal.Value.gt

