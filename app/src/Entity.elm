module Entity exposing (..)

{-|
    business : Entity
    business = Entity
      { name = "AAA Bakery"
      , entityType = TBusiness
      , account = Money.emptyAccount
      , inventory = []
      , position = Position 0 0
      , color = Color.rgb 1 0 0


-}

import CellGrid exposing(Position(..))
import Color exposing(Color)
import Money exposing(Account)





type Entity = Entity Common Characteristics


type alias Common = {
      name:  String
    , entityType : TEntity
    , account : Account
    , inventory : List Item
    , position : Position
    , color : Color
   }


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
