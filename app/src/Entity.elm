module Entity exposing (Entity(..), Common, Characteristics(..)
  , BusinessCharRecord
  , HouseholdCharRecord
  , ItemType(..)
  , ItemName(..)
  , TEntity(..)
  , getPosition
  , distance
  , getFiatAccount
  , setFiatAccount
  , inventorySize
  , fiatHoldingsOEntities
  , setCCAccount
  , getCCAccount
  , getType
  , setName
  , setPosition
  , getColor
  , setColor)

{-|
     business : Entity
     business = Entity
        { name = "AAA Bakery"
        , entityType = TBusiness
        , account = Money.emptyAccount cambiatus
        , inventory = []
        , position = Position 0 0
        , color = Color.rgb 1 0 0
       }
       (BusinessCharacteristics { radius = 10.0 })
-}

import CellGrid exposing(Position)
import Color exposing(Color)
import Money exposing(Account)




fiatHoldingsOEntities : Int -> List Entity -> Maybe Money.Value
fiatHoldingsOEntities t list =
  let
    mergedAccounts = list
      |> List.map getFiatAccount
      |> Money.mergeAccounts
  in
    case mergedAccounts of
        Just accounts -> Just (Money.value (Money.bankTime t) accounts)
        Nothing -> Nothing

type Entity = Entity Common Characteristics


type alias Common = {
      name:  String
    , entityType : TEntity
    , complementaryAccount : Account
    , fiatAccount : Account
    , inventory : List Item
    , position : Position
    , color : Color
   }


positionDistance : Position -> Position -> Float
positionDistance p q =
  let
      deltaX = p.row - q.row |> toFloat
      deltaY = p.column - q.column |> toFloat
  in
    sqrt (deltaX * deltaX + deltaY * deltaY)


distance : Entity -> Entity -> Float
distance (Entity common1 _)  (Entity common2 _)=
   positionDistance common1.position common2.position

getFiatAccount : Entity -> Money.Account
getFiatAccount (Entity common _) =
    common.fiatAccount

setFiatAccount : Account -> Entity -> Entity
setFiatAccount account (Entity common char) =
    Entity { common | fiatAccount = account} char

getCCAccount : Entity -> Money.Account
getCCAccount (Entity common _) =
    common.complementaryAccount

setCCAccount : Account -> Entity -> Entity
setCCAccount account (Entity common char) =
    Entity { common | complementaryAccount = account} char


setName : String -> Entity -> Entity
setName name (Entity common characteristics) =
    Entity { common | name = name } characteristics

getType : Entity ->TEntity
getType (Entity common _) =
    common.entityType

getPosition : Entity -> Position
getPosition   (Entity common characteristics) =
    common.position

setPosition : Int -> Int -> Entity -> Entity
setPosition i j (Entity common characteristics) =
    Entity { common | position = Position i j } characteristics



getColor : Entity -> Color
getColor (Entity common characteristics) =
    common.color


setColor : Float -> Float -> Float ->  Entity -> Entity
setColor r g b (Entity common characteristics) =
    Entity { common | color = Color.rgb r g b } characteristics

type alias Item = { itemType : ItemType, quantity : Quantity}

type ItemType = ItemType {
    itemName : ItemName
  , price : Money.Value
  }

type ItemName = AA | BB

type Quantity = Z Int | R Float


type TEntity = TShop | TSupplier | THousehold

type Characteristics =
    BusinessCharacteristics BusinessCharRecord | HouseholdCharacteristics HouseholdCharRecord

type alias BusinessCharRecord =
   {
      radius: Float

   }

type alias HouseholdCharRecord = {
  monthlyConsumptionA : Int
 }


inventorySize : Entity -> Int
inventorySize (Entity common _) =
       List.length common.inventory