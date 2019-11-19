module Entity exposing (Entity(..), Common, Characteristics(..)
  , BusinessCharRecord
  , HouseholdCharRecord
  , TEntity(..)
  , getPosition
  , setName
  , setPosition
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

setName : String -> Entity -> Entity
setName name (Entity common characteristics) =
    Entity { common | name = name } characteristics


getPosition : Entity -> Position
getPosition   (Entity common characteristics) =
    common.position

setPosition : Int -> Int -> Entity -> Entity
setPosition i j (Entity common characteristics) =
    Entity { common | position = Position i j } characteristics

setColor : Float -> Float -> Float ->  Entity -> Entity
setColor r g b (Entity common characteristics) =
    Entity { common | color = Color.rgb r g b } characteristics



type alias Item = { name : String, quantity : Quantity}

type Quantity = Z Int | R Float


type TEntity = TBusiness | THousehold

type Characteristics =
    BusinessCharacteristics BusinessCharRecord | HouseholdCharacteristics HouseholdCharRecord

type alias BusinessCharRecord =
   {
      radius: Float

   }

type alias HouseholdCharRecord = {
   whatever : String
 }
