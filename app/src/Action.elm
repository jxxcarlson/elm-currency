module Action exposing (payHouseholds, dailyActivity, consumeA, buyGoods)


import EngineData
import ActionHelper as AH
import State exposing(State)
import Utility
import Entity exposing(Entity)
import Inventory
import ModelTypes exposing(Inventory)


{-|

    import TestData
    import Report

    payHouseholds 0 TestData.initialState |> Report.fiatHoldingsDisplay 0
    --> "320 Real (F)"

-}
payHouseholds : Int -> State -> State
payHouseholds t state =
  if List.member (modBy 30 t) state.config.householdPayDays
  then
    let
        households = AH.creditHouseHolds t EngineData.config.periodicHouseHoldFiatIncome state.households
     in
        {state | households = households}
  else
    state

dailyActivity : Int -> State -> State
dailyActivity t state =
    state



consumeA : Int -> State -> State
consumeA t state =
     if List.member (modBy 30 t) state.config.householdPurchaseDays then
       let
           reduceInventoryOfA : Inventory -> Inventory
           reduceInventoryOfA inventory =
               Inventory.sub state.config.itemA inventory |> Tuple.second

           reduceInventoryOfHouseHold : Entity -> Entity
           reduceInventoryOfHouseHold e =
               Entity.mapInventory reduceInventoryOfA e


           newHouseholds = List.map reduceInventoryOfHouseHold state.households
       in
          {state | households = newHouseholds}
      else
        state


consumeA1 : Int -> State -> State
consumeA1 t state =
    Utility.applyToList consumeAForHouseHold state.households state
      |> Maybe.withDefault state


consumeAForHouseHold : Entity -> State -> State
consumeAForHouseHold houseHold state =
    state


buyGoods : Int -> State -> State
buyGoods t state =
    state

-- HELPERS --

