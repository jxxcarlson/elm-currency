module EngineData exposing (Config, businesses, config, generateHouseholds, supplier, business1, business2, initialHousehold)

import Entity exposing(Entity(..), TEntity(..)
  , Characteristics(..)
  , setName
  , setPosition
  , setColor
  )



import CellGrid exposing(Position)
import Color exposing(Color)
import Money
import Account exposing(Account)
import Random

type alias Config = {
   tickLoopInterval : Float
  , cycleLength : Int
  , renderWidth : Float
  ,  gridWidth : Int
  , maxHouseholds : Int
  , contentReleaseInterval : Int
  , numberOfTimesToWatchContent : Int
  , businessRadius : Float
  , itemPrice : Money.Value
  , monthlyItemConsumption : Int
  , householdPurchaseDays : List Int
  , fiatCurrencyName : String
  , periodicHouseHoldFiatIncome : Float
  , monthlyCCIncome : Money.Value
 }

config : Config
config = {
     tickLoopInterval = 1000
     , cycleLength = 30
    , renderWidth = 500
    , gridWidth = 30
    , maxHouseholds = 20
    , contentReleaseInterval = 15
    , numberOfTimesToWatchContent = 1
    , businessRadius = 10.0
    , itemPrice = Money.createValue fiatCurrency 2
    , monthlyItemConsumption = 8
    , householdPurchaseDays = [2, 6, 10, 14, 18, 22, 26, 30]
    , fiatCurrencyName = "Real"
    , periodicHouseHoldFiatIncome = 8.0
    , monthlyCCIncome = Money.createValue cambiatus 0
   }

cambiatus = Money.createCompCurrency "Cambiatus"

fiatCurrency = Money.createFiatCurrency "Real"

supplier : Entity
supplier = Entity
   { name = "X"
   , entityType = TSupplier
   , complementaryAccount = Account.empty cambiatus
   , fiatAccount = Account.empty fiatCurrency
   , inventory = []
   , position = Position (config.gridWidth - 5) (config.gridWidth - 5) -- Position (config.gridWidth - 5) (config.gridWidth - 5)
   , color = Color.rgb 0.4 0.4 1
  }
  (BusinessCharacteristics { radius = config.businessRadius })


business1 : Entity
business1 = Entity
   { name = "A"
   , entityType = TShop
   , complementaryAccount = Account.empty cambiatus
   , fiatAccount = Account.empty fiatCurrency
   , inventory = []
   , position = Position 8 (config.gridWidth - 5)
   , color = Color.rgb 0.8 0 0.3
  }
  (BusinessCharacteristics { radius = config.businessRadius })


business2 =
   business1
     |> setName "B"
     |> setPosition (config.gridWidth - 5) 5
     |> setColor 0.8 0 0.3



businesses = [business1, business2, supplier]

initialHousehold : Entity
initialHousehold =
  Entity
  { name = "1"
   , entityType = THousehold
   , complementaryAccount = Account.empty cambiatus
   , fiatAccount = Account.empty fiatCurrency
   , inventory = []
   , position = Position 20 15
   , color = Color.rgb 0.8 0.8 0.6
  }
  (HouseholdCharacteristics { monthlyConsumptionA = 0 })


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


