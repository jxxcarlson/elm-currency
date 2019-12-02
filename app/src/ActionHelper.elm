module ActionHelper exposing (..)


import Account
import EngineData
import Entity exposing(Entity, Item)
import Random
import Random.List
import Money
import State exposing(State)



householdConsumptionStep : Int -> State -> State
householdConsumptionStep t state =
    state


--purchase : Item -> Entity -> Entity  -> (Entity, Entity)
--purchase item purchaser

{-|
    import TestData
    import EngineData
    import Entity

    nearestShop EngineData.initialHousehold  TestData.initialState
    --> Just EngineData.business2

    (List.map (Entity.distance EngineData.supplier)) (getShops TestData.initialState)
    --> [17, 20]
-}
nearestShop : Entity -> State -> Maybe Entity
nearestShop e state =
    let
        shops = getShops state
        distances = (List.map (Entity.distance e)) shops
        shopswithDistances = List.map2 Tuple.pair shops distances
        m = List.minimum distances |> Maybe.withDefault 0
        closestShops = List.filter (\(s, d) -> abs(d - m) < 0.001) shopswithDistances |> List.map Tuple.first
    in
        List.head closestShops


{-|
    import TestData

    getShops TestData.initialState |> List.length
    --> 2
-}
getShops : State -> List Entity
getShops state =
    List.filter (\e -> Entity.getType e == Entity.TShop) state.businesses


{-|
Select a household at random from those with the least
consumption of A.

    import TestData
    import EngineData
    import Entity exposing(TEntity(..))

    selectHouseholdWithLeastInventory 0 TestData.initialState |> Tuple.second |> Maybe.map Entity.getType
    --> Just THousehold

-}
selectHouseholdWithLeastInventory : Int -> State -> (State, Maybe Entity)
selectHouseholdWithLeastInventory t state =
  let
      m = minimumHouseholdInventory state
      candidates = List.filter (\h -> Entity.inventorySize h == m) state.households
      (result, newSeed) = Random.step (Random.List.choose candidates) state.seed
  in
    ({state | seed = newSeed }, Tuple.first result)


{-|

    import TestData

    minimumHouseholdInventory TestData.initialState
    --> 0
-}
minimumHouseholdInventory : State -> Int
minimumHouseholdInventory state =
    state.households
      |> List.map Entity.inventorySize
      |> List.minimum
      |> Maybe.withDefault 0



creditHouseHolds : Int -> Float -> List Entity -> List Entity
creditHouseHolds t value households =
   List.map (creditHousehold t value) households


creditHousehold : Int -> Float -> Entity -> Entity
creditHousehold t value entity =
    let
        currency = Money.createFiatCurrency EngineData.config.fiatCurrencyName
        money = Money.createInfinite currency 0 EngineData.config.monthlyFiatIncome
        account = Account.credit (Money.bankTime t) money (Entity.getFiatAccount entity)
    in
        Entity.setFiatAccount account entity

