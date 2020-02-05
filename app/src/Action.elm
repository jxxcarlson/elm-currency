module Action exposing (businessBuyGoods, consumeA, dailyActivity, householdBuyGoods, payHouseholds)

import Account
import ActionHelper as AH
import EngineData exposing (Config)
import Entity exposing (Entity)
import Inventory
import List.Extra
import ModelTypes exposing (Inventory)
import Money
import Random
import State exposing (State)
import Utility
import Value


{-|

    import TestData
    import Report

    payHouseholds 0 TestData.initialState |> Report.fiatHoldingsDisplay 0
    --> "320 Real (F)"

-}
payHouseholds : Config -> Int -> State -> State
payHouseholds config t state =
    if List.member (modBy 30 t) state.config.householdPayDays then
        let
            households =
                AH.creditHouseHolds config t config.periodicHouseHoldFiatIncome state.households
        in
        { state | households = households }

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
                if Entity.inventoryAmount "AA" e > 0 then
                    Entity.mapInventory reduceInventoryOfA e

                else
                    e

            positiveInventoryHouseholds =
                List.filter (\e -> Entity.inventoryAmount "AA" e > 0) state.households

            newHouseholds =
                List.map reduceInventoryOfHouseHold state.households
        in
        { state
            | households = newHouseholds
            , totalHouseholdConsumption = state.totalHouseholdConsumption + List.length positiveInventoryHouseholds
        }

    else
        state


initializeSupplier : State -> State
initializeSupplier state =
    state



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
        lowInventoryBusiness =
            List.filter
                (\e -> Entity.inventoryAmount "AA" e < state.config.minimumBusinessInventoryOfA)
                state.businesses
    in
    case List.head lowInventoryBusiness of
        Nothing ->
            state

        Just business ->
            let
                maxRandInt =
                    1000

                ( i, newSeed ) =
                    Random.step (Random.int 0 maxRandInt) state.seed

                randomPurchaseAmount : Int -> Int
                randomPurchaseAmount ri =
                    let
                        p =
                            toFloat ri / toFloat maxRandInt

                        range =
                            toFloat (state.config.maximumPurchaseOfA - state.config.minimumPurchaseOfA)
                    in
                    state.config.minimumPurchaseOfA + round (p * range)

                a =
                    randomPurchaseAmount i

                item =
                    ModelTypes.setQuantity a state.config.itemA

                newBusiness =
                    Entity.addToInventory item business

                newBusinesses =
                    List.Extra.updateIf
                        (\b -> Entity.getName b == Entity.getName newBusiness)
                        (\b -> newBusiness)
                        state.businesses
            in
            { state | seed = newSeed, businesses = newBusinesses }


{-| Choose a low-inventory household at random and buy goods
-}
householdBuyGoods : Int -> State -> State
householdBuyGoods t state =
    let
        sortByAccountValue : Entity -> Int
        sortByAccountValue e =
            Entity.getFiatAccount e
                |> Account.value (Money.bankTime t)
                |> Value.intValue
                |> (\v -> -v)

        orderedHouseholds =
            List.sortBy (\e -> sortByAccountValue e) state.households
                |> List.take 5

        n =
            List.length orderedHouseholds - 1

        ( i, newSeed ) =
            Random.step (Random.int 0 n) state.seed
    in
    case List.Extra.getAt 0 orderedHouseholds of
        Nothing ->
            { state | seed = newSeed }

        Just e ->
            householdBuyGoods_ t e { state | seed = newSeed }


householdBuyGoods_ : Int -> Entity -> State -> State
householdBuyGoods_ t e state =
    case AH.nearestShop e state of
        Nothing ->
            state

        Just shop ->
            let
                maxRandInt =
                    1000

                ( i, newSeed ) =
                    Random.step (Random.int 0 maxRandInt) state.seed

                randomPurchaseAmount : Int -> Int
                randomPurchaseAmount ri =
                    let
                        p =
                            toFloat ri / toFloat maxRandInt

                        range =
                            toFloat (state.config.householdMaximumPurchaseAmount - state.config.householdMinimumPurchaseAmount)
                    in
                    state.config.householdMinimumPurchaseAmount + round (p * range)

                a =
                    randomPurchaseAmount i

                qS =
                    Entity.inventoryAmount "AA" shop

                qH =
                    Entity.inventoryAmount "AA" e

                qP =
                    if qH >= state.config.householdLowInventoryThreshold then
                        -- Don't purchase itemA if already have enough on hand
                        0

                    else if a > qS then
                        -- Can't purchase more than is available in shop
                        qS

                    else
                        a

                item =
                    ModelTypes.setQuantity qP state.config.itemA

                itemPrice =
                    Money.mul qP state.config.itemAMoney

                addInventoryOfA : Inventory -> Inventory
                addInventoryOfA inventory =
                    Inventory.add item inventory

                addInventoryOfEntity : Entity -> Entity
                addInventoryOfEntity e_ =
                    Entity.mapInventory addInventoryOfA e_

                subInventoryOfA : Inventory -> Inventory
                subInventoryOfA inventory =
                    Inventory.sub item inventory
                        |> Tuple.second

                subInventoryOfEntity : Entity -> Entity
                subInventoryOfEntity e_ =
                    Entity.mapInventory subInventoryOfA e_

                debitAccount : Account.Account -> Account.Account
                debitAccount =
                    \account -> Account.debit (Money.bankTime t) itemPrice account

                creditAccount : Account.Account -> Account.Account
                creditAccount =
                    \account -> Account.debit (Money.bankTime t) itemPrice account

                newHousehold =
                    addInventoryOfEntity e
                        |> Entity.mapAccount debitAccount (Entity.getFiatAccount e)

                newBusiness =
                    subInventoryOfEntity shop
                        |> Entity.mapAccount creditAccount (Entity.getFiatAccount shop)

                newHouseholds =
                    List.Extra.updateIf
                        (\e1 -> Entity.getName e1 == Entity.getName e)
                        (\_ -> newHousehold)
                        state.households

                newBusinesses =
                    List.Extra.updateIf
                        (\e1 -> Entity.getName e1 == Entity.getName shop)
                        (\_ -> newBusiness)
                        state.businesses
            in
            { state
                | households = newHouseholds
                , businesses = newBusinesses
                , seed = newSeed
                , totalHouseholdPurchases = state.totalHouseholdPurchases + qP
            }


householdBuyGoods1 : Int -> State -> State
householdBuyGoods1 t state =
    if List.member (modBy 30 t) state.config.householdPurchaseDays then
        let
            addInventoryOfA : Inventory -> Inventory
            addInventoryOfA inventory =
                Inventory.add state.config.itemA inventory

            addInventoryOfHouseHold : Entity -> Entity
            addInventoryOfHouseHold e =
                Entity.mapInventory addInventoryOfA e

            newHouseholds =
                List.map addInventoryOfHouseHold state.households
        in
        { state | households = newHouseholds }

    else
        state



-- HELPERS --
