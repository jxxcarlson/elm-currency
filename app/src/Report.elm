module Report exposing (fiatHoldingsDisplay, numberOfInventoriesBelow, householdInventoryOf, minMaxHouseholdInventoryOf, businessInventoryOf)

import Entity exposing(Entity)
import Money
import ModelTypes exposing(itemQuantity, itemName)
import State exposing(State)
import Maybe.Extra



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


minMaxHouseholdInventoryOf : String -> State -> List Int
minMaxHouseholdInventoryOf itemName_ state =
   let
       f : Entity -> Int
       f e =
            List.filter (\i -> (itemName i) == itemName_) (Entity.inventory e)
              |> List.map itemQuantity
              |> List.sum

       inventories : List Int
       inventories = List.map f state.households
   in
     [List.minimum inventories,  List.maximum inventories]
       |> Maybe.Extra.values

numberOfInventoriesBelow : String -> Int -> State -> Int
numberOfInventoriesBelow itemName_ k state =
  let
         f : Entity -> Int
         f e =
              List.filter (\i -> (itemName i) == itemName_) (Entity.inventory e)
                |> List.map itemQuantity
                |> List.sum

         inventories : List Int
         inventories = List.map f state.households
  in
       List.filter (\i -> i <= k) inventories |> List.length

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
