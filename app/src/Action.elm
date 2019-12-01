module Action exposing (..)


import Account
import EngineData
import Entity exposing(Entity)
import Random
import Random.List
import Money
import State exposing(State)

dailyActivity : Int -> State -> State
dailyActivity t state =
    state


householdConsumptionStep : Int -> State -> State
householdConsumptionStep t state =
    state


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

getShops : State -> List Entity
getShops state =
    List.filter (\e -> Entity.getType e == Entity.TShop) state.businesses


{-|
Select a household at random from those with the least
consumption of A.
-}
selectHouseholdWithLeastInventory : Int -> State -> (State, Maybe Entity)
selectHouseholdWithLeastInventory t state =
  let
      m = minimumHouseholdInventory state
      candidates = List.filter (\h -> Entity.inventorySize h == m) state.households
      (result, newSeed) = Random.step (Random.List.choose candidates) state.seed
  in
    ({state | seed = newSeed }, Tuple.first result)




minimumHouseholdInventory : State -> Int
minimumHouseholdInventory state =
    state.households
      |> List.map Entity.inventorySize
      |> List.minimum
      |> Maybe.withDefault 0




payHouseholds : Int -> State -> State
payHouseholds t state =
    let
        households = creditHouseHolds t EngineData.config.monthlyFiatIncome state.households
     in
        {state | households = households}


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


