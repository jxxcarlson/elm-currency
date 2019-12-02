module Value exposing (create, gt, gte)

import Internal.Types exposing(Value, Currency)
import Internal.Value

create : Currency -> Float -> Value
create = Internal.Value.create

gte : Value -> Value -> Maybe Bool
gte  = Internal.Value.gte

gt : Value -> Value -> Maybe Bool
gt  = Internal.Value.gt

