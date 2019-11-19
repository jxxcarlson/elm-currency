module EngineData exposing (businesses)

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
    , maxHouseholds = 30
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
     |> setColor 0 0 1

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
    s = lastHouseHoldGeneratorState i
  in
    s.households

lastHouseHoldGeneratorState :   HouseHoldGeneratorState -> HouseHoldGeneratorState
lastHouseHoldGeneratorState s =
    if List.length s.households == s.maxHouseHolds then
      s
    else
      let
         (i, seed1) = Random.step (Random.int 0 config.gridWidth) s.seed
         (j, seed2) = Random.step (Random.int 0 config.gridWidth) seed1
      in
        if List.member (Position i j) s.existingPositions then
          s
        else
          let
              newCount = s.count + 1
              newName = String.fromInt newCount
              newHousehold = initialHousehold |> setPosition i j |> setName newName
          in
            {s | count = newCount

              , existingPositions = (Position i j) :: s.existingPositions
              , households = newHousehold :: s.households}
