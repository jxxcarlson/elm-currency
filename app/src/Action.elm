module Action exposing (payHouseholds, dailyActivity)


import EngineData
import ActionHelper as AH
import State exposing(State)


{-|

    import TestData
    import Report

    payHouseholds 0 TestData.initialState |> Report.fiatHoldingsDisplay 0
    --> "320 Real (F)"

-}
payHouseholds : Int -> State -> State
payHouseholds t state =
    let
        households = AH.creditHouseHolds t EngineData.config.monthlyFiatIncome state.households
     in
        {state | households = households}

dailyActivity : Int -> State -> State
dailyActivity t state =
    state



-- HELPERS --
