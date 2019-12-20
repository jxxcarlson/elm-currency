module Action exposing (payHouseholds, dailyActivity, consumeA, householdBuyGoods, businessBuyGoods)


import EngineData
import ActionHelper as AH
import State exposing(State)
import Utility
import Entity exposing(Entity)
import Inventory
import ModelTypes exposing(Inventory)
import Random
import List.Extra

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
     if List.member (modBy 30 t) state.config.householdConsumptionDays then
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

initializeSupplier : State -> State
initializeSupplier state = state



--consumeA1 : Int -> State -> State
--consumeA1 t state =
--    Utility.applyToList consumeAForHouseHold state.households state
--      |> Maybe.withDefault state
--
--
--consumeAForHouseHold : Entity -> State -> State
--consumeAForHouseHold houseHold state =
--    state




businessBuyGoods : State -> State
businessBuyGoods state =
    let
        lowInventoryBusiness = List.filter (\e -> Entity.inventoryAmount "AA" e < state.config.minimumBusinesInventoryOfA) state.businesses
    in
    case List.head  lowInventoryBusiness of
        Nothing -> state
        Just business ->
          let
            maxRandInt = 1000

            (i, newSeed) = Random.step (Random.int 0 maxRandInt) state.seed

            randomPurchaseAmount : Int -> Int
            randomPurchaseAmount ri =
                let
                   p = (toFloat ri)/(toFloat maxRandInt)
                   range = toFloat (state.config.maximumPurchaseOfA - state.config.minimumPurchaseOfA)
                in
                   state.config.minimumPurchaseOfA + round(p * range)

            a = randomPurchaseAmount i

            item = ModelTypes.setQuantity a state.config.itemA

            newBusiness = Entity.addToInventory item business
            newBusinesses = List.Extra.updateIf
                (\b -> Entity.getName b == Entity.getName newBusiness)
                (\b -> newBusiness)
                state.businesses

          in
           { state | seed = newSeed, businesses = newBusinesses}




householdBuyGoods : Int -> State -> State
householdBuyGoods t state =
    if List.member (modBy 30 t) state.config.householdPurchaseDays then
        let
                addInventoryOfA : Inventory -> Inventory
                addInventoryOfA inventory =
                    Inventory.add state.config.itemA inventory

                addInventoryOfHouseHold : Entity -> Entity
                addInventoryOfHouseHold e =
                    Entity.mapInventory addInventoryOfA e


                newHouseholds = List.map addInventoryOfHouseHold state.households
        in
               {state | households = newHouseholds}
      else
             state

-- HELPERS --

