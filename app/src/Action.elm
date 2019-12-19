module Action exposing (payHouseholds, dailyActivity, buyA)


import EngineData
import ActionHelper as AH
import State exposing(State)
import Utility
import Entity exposing(Entity)


{-|

    import TestData
    import Report

    payHouseholds 0 TestData.initialState |> Report.fiatHoldingsDisplay 0
    --> "320 Real (F)"

-}
payHouseholds : Int -> State -> State
payHouseholds t state =
  if List.member (modBy 30 t) state.config.householdPurchaseDays then
    let
        households = AH.creditHouseHolds t EngineData.config.periodicHouseHoldFiatIncome state.households
     in
        {state | households = households}
  else
    state

dailyActivity : Int -> State -> State
dailyActivity t state =
    state

buyA : Int -> State -> State
buyA t state =
    Utility.applyToList buyAForHouseHold state.households state
      |> Maybe.withDefault state


buyAForHouseHold : Entity -> State -> State
buyAForHouseHold houseHold state =
    state

-- HELPERS --

