module Report exposing (fiatHoldingsDisplay, householdInventoryOf, businessInventoryOf)

import Entity exposing(Entity)
import Money
import ModelTypes exposing(itemQuantity, itemName)
import State exposing(State)



fiatHoldingsDisplay t state =
    case Entity.fiatHoldingsOEntities t state.households  of
        Just value ->  Money.valueToString value
        Nothing -> "--"


householdInventoryOf : String -> State -> Int
householdInventoryOf itemName_ state =
   let
       f : Entity -> Int
       f e =
            List.filter (\i -> (itemName i) == itemName_) (Entity.inventory e)
              |> List.map itemQuantity
              |> List.sum
   in
      List.map f state.households
        |> List.sum


businessInventoryOf : String -> State -> List Int
businessInventoryOf itemName_ state =
   let
       f : Entity -> Int
       f e =
            List.filter (\i -> (itemName i) == itemName_) (Entity.inventory e)
              |> List.map itemQuantity
              |> List.sum
   in
      List.map f state.businesses
