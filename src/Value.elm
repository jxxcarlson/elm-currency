module Value exposing (..)

import Internal.Types exposing(Value, Currency)
import Internal.Value

create : Currency -> Float -> Value
create = Internal.Value.create



