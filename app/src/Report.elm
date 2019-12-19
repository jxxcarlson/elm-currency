module Report exposing (fiatHoldingsDisplay, inventoryOf)

import Entity exposing(Entity)
import Money
import ModelTypes exposing(itemQuantity, itemName)
import State exposing(State)



fiatHoldingsDisplay t state =
    case Entity.fiatHoldingsOEntities t state.households  of
        Just value ->  Money.valueToString value
        Nothing -> "--"


inventoryOf : String -> State -> Int
inventoryOf itemName_ state =
   let
       f : Entity -> Int
       f e =
            List.filter (\i -> (itemName i) == itemName_) (Entity.inventory e)
              |> List.map itemQuantity
              |> List.sum
   in
      List.map f state.households
        |> List.sum

