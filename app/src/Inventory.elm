module Inventory exposing (Inventory, Item(..), mapItem, map2Item, add, sub, addToItem , subFromItem, InventoryError(..))


import Money
import Basics

type alias Inventory = List Item

type InventoryError = InventoryOpSuccess | NoSuchItem | InsufficientQuantity

{-|  In this model, we assume that the name
and price of an item are constant.  That is,
the operations of `addItem` and `subItem`
do not affect these quantities.


-}
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
    i1 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 1  }

    mapItem (\x -> 2*x) i1
    --> Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 2  }

-}
mapItem : (Int -> Int) -> Item -> Item
mapItem  f (Item data) =
    Item { data | quantity =  f data.quantity }


{-|

    import Internal.Types exposing(..)
    import Internal.Money

    i1 : Item
    i1 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 1  }

    map2Item (+) i1 i1
    --> Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 2  }

-}
map2Item : (Int -> Int -> Int ) -> Item -> Item -> Item
map2Item  f (Item data1) (Item data2) =
    Item { data1 | quantity =  f data1.quantity data2.quantity }


{-|  If the name and price of the target agree,
add the quantity of the source to the target
and return the target. Otherwise, return the target.

    import Internal.Types exposing(..)
    import Internal.Money

    i1 : Item
    i1 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 1 }

    i2 : Item
    i2 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 2 }

    jj : Item
    jj = Item {name = "BB", price = Value Internal.Money.usDollars (Cents 100), quantity = 2 }

    subFromItem i1 i2
    --> Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 1  }

    subFromItem jj i2
    --> Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 2  }
 -}
subFromItem : Item -> Item -> Item
subFromItem source target =
    case (name source == name target, price source == price target)  of
        (True, True) -> map2Item (-)  target source
        (_, _) -> target


{-|  If the name and price of the target agree,
add the quantity of the source to the target
and return the target. Otherwise, return the target.

    import Internal.Types exposing(..)
    import Internal.Money

    i1 : Item
    i1 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 1  }

    i2 : Item
    i2 = Item {name = "B", price = Value Internal.Money.usDollars (Cents 100), quantity = 1  }

    addToItem i1 i1
    --> Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 2  }

    addToItem i1 i2
    --> i2

-}
addToItem : Item -> Item -> Item
addToItem source target =
    case (name source == name target, price source == price target)  of
        (True, True) -> map2Item (+) source target
        (_, _) -> target



{-|

    import Internal.Types exposing(..)
    import Internal.Money

    i1 : Item
    i1 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 1  }

    add i1 []
    --> [i1]

    add i1 [i1]
    --> [Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 2  }]

-}
add : Item -> Inventory -> Inventory
add item inventory  =
   case List.filter (\i -> name i == name item && price i == price item) inventory |> List.head of
       Nothing -> item::inventory
       Just target ->
           List.map (\targetItem -> addToItem item targetItem) inventory


{-|

    import Internal.Types exposing(..)
    import Internal.Money

    i0 : Item
    i0 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 0 }

    i1 : Item
    i1 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 1 }

    i2 : Item
    i2 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 2 }


    sub i1 []
    --> (NoSuchItem, [])

    sub i1 [i1]
    --> (InventoryOpSuccess, [i0])

    sub i1 [i2]
    --> (InventoryOpSuccess, [i1])

    sub i2 [i1]
    --> (InsufficientQuantity, [i0])

-}
sub : Item -> Inventory -> (InventoryError, Inventory)
sub item inventory  =
   case List.filter (\i -> name i == name item && price i == price item) inventory |> List.head of
       Nothing -> (NoSuchItem, inventory)
       Just target ->
           if quantity target >= quantity item then
             (InventoryOpSuccess, List.map (\targetItem -> subFromItem item targetItem) inventory)
           else
             let
                 adjustedItem = mapItem (\q -> quantity target) item
             in
                 (InsufficientQuantity, List.map (\targetItem -> subFromItem adjustedItem targetItem) inventory)

