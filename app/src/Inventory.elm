module Inventory exposing (Inventory, Item, add )


import Money
import Basics

type alias Inventory = List Item

type Item = Item {
     name : String
   , price : Money.Value
   , quantity : Int}


name : Item -> String
name (Item data) =
    data.name

price : Item -> Money.Value
price (Item data) =
    data.price

quantity : Item -> Int
quantity (Item data) =
    data.quantity

{-|

    import Internal.Types exposing(..)
    import Internal.Money

    i1 : Item
    i1 = Item {name = "AA", price = Value Money.usDollars (Cents 100) }

    add i1 []
    -> 3


-}
add : Item -> Inventory -> Inventory
add item inventory  =
   case List.filter (\i -> name i == name item && price i == price item) inventory |> List.head of
       Nothing -> item::inventory
       Just target ->
           let
             updatedItem = Item { quantity = quantity target + quantity item, name = name target, price = price target}
           in
            updatedItem::(List.filter (\i -> i /= item) inventory)

sub : Item -> Inventory -> Inventory
sub item inventory  =
   case List.filter (\i -> name i == name item && price i == price item) inventory |> List.head of
       Nothing -> inventory
       Just target ->
           let
             updatedItem = Item { quantity = max (quantity target - quantity item) 0, name = name target, price = price target}
           in
            updatedItem::(List.filter (\i -> i /= item) inventory)


-- sub : Item -> Inventory -> Inventory



simplify : Inventory -> Inventory
simplify = identity

