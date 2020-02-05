module Report exposing
    ( businessInventoryOf
    , ccBalanceOf
    , fiatBalanceOf
    , fiatHoldingsDisplay
    , householdInventoryOf
    , minMaxHouseholdInventoryOf
    , numberOfInventoriesBelow
    )

import Account
import Entity exposing (Entity)
import Internal.Types exposing (BankTime)
import Maybe.Extra
import ModelTypes exposing (itemName, itemQuantity)
import Money
import State exposing (State)
import Value


fiatHoldingsDisplay t state =
    case Entity.fiatHoldingsOEntities t state.households of
        Just value ->
            Money.valueToString value

        Nothing ->
            "--"


householdInventoryOf : String -> State -> Int
householdInventoryOf itemName_ state =
    let
        f : Entity -> Int
        f e =
            List.filter (\i -> itemName i == itemName_) (Entity.inventory e)
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
            List.filter (\i -> itemName i == itemName_) (Entity.inventory e)
                |> List.map itemQuantity
                |> List.sum

        inventories : List Int
        inventories =
            List.map f state.households
    in
    [ List.minimum inventories, List.maximum inventories ]
        |> Maybe.Extra.values


numberOfInventoriesBelow : String -> Int -> State -> Int
numberOfInventoriesBelow itemName_ k state =
    let
        f : Entity -> Int
        f e =
            List.filter (\i -> itemName i == itemName_) (Entity.inventory e)
                |> List.map itemQuantity
                |> List.sum

        inventories : List Int
        inventories =
            List.map f state.households
    in
    List.filter (\i -> i <= k) inventories |> List.length


businessInventoryOf : String -> State -> List Int
businessInventoryOf itemName_ state =
    let
        f : Entity -> Int
        f e =
            List.filter (\i -> itemName i == itemName_) (Entity.inventory e)
                |> List.map itemQuantity
                |> List.sum
    in
    List.map f state.businesses


fiatBalanceOf : BankTime -> List Entity -> List Float
fiatBalanceOf bt entityList =
    let
        f : Entity -> Float
        f e =
            e
                |> Entity.getFiatAccount
                |> Account.value bt
                |> Value.toFloat_
    in
    List.map f entityList


ccBalanceOf : BankTime -> List Entity -> List Float
ccBalanceOf bt entityList =
    let
        f : Entity -> Float
        f e =
            e
                |> Entity.getCCAccount
                |> Account.value bt
                |> Value.toFloat_
    in
    List.map f entityList
