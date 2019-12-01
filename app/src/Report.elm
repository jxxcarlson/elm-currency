module Report exposing (fiatHoldingsDisplay)

import Entity
import Money



fiatHoldingsDisplay t state =
    case Entity.fiatHoldingsOEntities t state.households  of
        Just value ->  Money.valueToString value
        Nothing -> "--"