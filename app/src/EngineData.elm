module EngineData exposing (businesses, config, generateHouseholds)

import Entity exposing(Entity(..), TEntity(..)
  , Characteristics(..)
  , setName
  , setPosition
  , setColor
  )



import CellGrid exposing(Position)
import Color exposing(Color)
import Money exposing(Account)
import Random

config = {
    gridWidth = 50
    , maxHouseholds = 60
   }

cambiatus = Money.createCompCurrency "Cambiatus"

real = Money.createFiatCurrency "Real"


business1 : Entity
business1 = Entity
   { name = "A"
   , entityType = TBusiness
   , complementaryAccount = Money.emptyAccount cambiatus
   , fiatAccount = Money.emptyAccount real
   , inventory = []
   , position = Position 5 5
   , color = Color.rgb 1 0 0
  }
  (BusinessCharacteristics { radius = 10.0 })


business2 =
   business1
     |> setName "B"
     |> setPosition 30 30
     |> setColor 1 0 0

businesses = [business1, business2]

initialHousehold : Entity
initialHousehold =
  Entity
  { name = "1"
   , entityType = THousehold
   , complementaryAccount = Money.emptyAccount cambiatus
   , fiatAccount = Money.emptyAccount real
   , inventory = []
   , position = Position 20 15
   , color = Color.rgb 1 1 0
  }
  (HouseholdCharacteristics { whatever = "hohoho!" })


type alias HouseHoldGeneratorState =
    {  count : Int
       , seed : Random.Seed
      , existingPositions : List CellGrid.Position
      , households : List Entity
      , maxHouseHolds : Int
     }

initialHouseHoldGeneratorState : Int ->  Int -> HouseHoldGeneratorState
initialHouseHoldGeneratorState k maxHouseHolds =
    let
      (i, seed1) = Random.step (Random.int 0 config.gridWidth) (Random.initialSeed k)
      (j, seed2) = Random.step (Random.int 0 config.gridWidth) seed1
      existingPositions = (Entity.getPosition initialHousehold) :: (List.map Entity.getPosition businesses)

    in
      {  count = 1
      , seed = seed2
      , existingPositions = existingPositions
      , households = [initialHousehold]
      , maxHouseHolds = maxHouseHolds
      }


generateHouseholds : Int -> Int -> List Entity
generateHouseholds intSeed numberOfHouseholds =
  let
    i = initialHouseHoldGeneratorState intSeed config.maxHouseholds
    s = List.foldl newState i (List.range 2  numberOfHouseholds)
  in
    s.households



newState : Int -> HouseHoldGeneratorState -> HouseHoldGeneratorState
newState k s =
    let
         (i, seed1) = Random.step (Random.int 0 config.gridWidth) s.seed
         (j, seed2) = Random.step (Random.int 0 config.gridWidth) seed1
         newCount = s.count + 1
         newName = String.fromInt newCount
         newHousehold = initialHousehold |> setPosition i j |> setName newName
     in
       {s | count = newCount
          , seed = seed2
          , existingPositions = (Position i j) :: s.existingPositions
          , households = newHousehold :: s.households}


