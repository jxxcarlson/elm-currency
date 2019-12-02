module Entity exposing (Entity(..), Common, Characteristics(..)
  , BusinessCharRecord
  , HouseholdCharRecord
  , Item
  , TEntity(..)
  , complementaryAccount
  , fiatAccount
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
  , selectAccount
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
import Money exposing(Money)
import Account exposing(Account)




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

complementaryAccount : Entity -> Account
complementaryAccount (Entity common _) =
    common.complementaryAccount

fiatAccount : Entity -> Account
fiatAccount (Entity common _) =
    common.fiatAccount

selectAccount : Money -> Entity -> Maybe Account
selectAccount money (Entity common _) =
     if Money.currency money == Account.currency common.fiatAccount then Just <| common.fiatAccount
             else if Money.currency money == Account.currency common.complementaryAccount then Just <|common.complementaryAccount
             else Nothing

type Item = Item {
     itemName : String
   , price : Money.Value
   , quantity : Int}



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


fiatHoldingsOEntities : Int -> List Entity -> Maybe Money.Value
fiatHoldingsOEntities t list =
  let
    mergedAccounts = list
      |> List.map getFiatAccount
      |> Account.merge
  in
    case mergedAccounts of
        Just accounts -> Just <| Account.value (Money.bankTime t) accounts
        Nothing -> Nothing


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

getFiatAccount : Entity -> Account
getFiatAccount (Entity common _) =
    common.fiatAccount

setFiatAccount : Account -> Entity -> Entity
setFiatAccount account (Entity common char) =
    Entity { common | fiatAccount = account} char

getCCAccount : Entity -> Account
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


inventorySize : Entity -> Int
inventorySize (Entity common _) =
       List.length common.inventory