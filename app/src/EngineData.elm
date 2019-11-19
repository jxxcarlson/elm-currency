module EngineData exposing (..)

import Entity exposing(Entity(..), TEntity(..), Characteristics(..))
import CellGrid exposing(Position)
import Color exposing(Color)
import Money exposing(Account)


cambiatus = Money.createCompCurrency "Cambiatus"


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