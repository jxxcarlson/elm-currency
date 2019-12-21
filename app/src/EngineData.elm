module EngineData exposing (Config, businesses, config, generateHouseholds, supplier, suppliers, business1, business2, initialHousehold)

import Entity exposing(Entity(..), TEntity(..)
  , Characteristics(..)
  , setName
  , setPosition
  , setColor
  )



import CellGrid exposing(Position)
import Color exposing(Color)
import Money exposing(Value, Money)
import Account exposing(Account)
import Random
import ModelTypes exposing (Item(..))


type alias Config = {
   title : String
  , tickLoopInterval : Float
  , cycleLength : Int
  , renderWidth : Float
  , gridWidth : Int
  -- Financial
  , fiatCurrency : Money.Currency
  , fiatCurrencyName : String
  -- Businesses
  , contentReleaseInterval : Int
  , numberOfTimesToWatchContent : Int
  , businessRadius : Float
  , itemPrice : Money.Value
  , itemA : Item
  , itemAMoney : Money
  , randomPurchaseFraction : Float
  , minimumBusinessInventoryOfA : Int
  , minimumPurchaseOfA : Int
  , maximumPurchaseOfA : Int


  -- Households
  , numberOfHouseholds : Int
  , monthlyItemConsumption : Int
  , householdPurchaseDays : List Int
  , householdMinimumPurchaseAmount : Int
  , householdMaximumPurchaseAmount : Int
  , householdLowInventoryThreshold : Int
  , householdConsumptionDays : List Int
  , householdPayDays : List Int
  , periodicHouseHoldFiatIncome : Float
  , monthlyCCIncome : Money.Value
 }

config : Config
config =
   {
      title = "Simple test, Fiat currency"
     , tickLoopInterval = 100
    , cycleLength = 360
    , renderWidth = 500
    , gridWidth = 30
     -- Financial
    , fiatCurrency = fiatCurrency
    , fiatCurrencyName = "Real"
     -- Businesses
    , contentReleaseInterval = 15
    , numberOfTimesToWatchContent = 1
    , businessRadius = 10.0
    , itemPrice = Money.createValue fiatCurrency 2
    , itemA = Item {name = "AA", price = Money.createValue fiatCurrency 2.0, quantity = 1  }
    , itemAMoney = Money.createInfinite fiatCurrency 0 2.0
    , randomPurchaseFraction = 0.1
    , minimumBusinessInventoryOfA = 20
    , minimumPurchaseOfA = 5
    , maximumPurchaseOfA = 15

    -- Households
    , numberOfHouseholds = 20
    , monthlyItemConsumption = 8
    , householdPurchaseDays = [1, 5, 9, 13, 17, 21, 25, 28] -- not used
    , householdMinimumPurchaseAmount = 2
    , householdMaximumPurchaseAmount = 5
    , householdLowInventoryThreshold = 2
    , householdConsumptionDays = [3, 7, 12, 15, 19, 23, 26, 29]
    , householdPayDays = [1, 15]
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
   , inventory = [ModelTypes.setQuantity 20 config.itemA]
   , position = Position 8 (config.gridWidth - 5)
   , color = Color.rgb 0.8 0 0.3
  }
  (BusinessCharacteristics { radius = config.businessRadius })


business2 =
   business1
     |> setName "B"
     |> setPosition (config.gridWidth - 5) 5
     |> setColor 0.8 0 0.3



businesses = [business1, business2]

suppliers = [supplier]

initialHousehold : Entity
initialHousehold =
  Entity
  { name = "1"
   , entityType = THousehold
   , complementaryAccount = Account.empty cambiatus
   , fiatAccount = Account.empty fiatCurrency |> Account.credit (Money.bankTime 0)  (Money.createInfinite fiatCurrency 0 4 )
   , inventory =  [ModelTypes.setQuantity 4 config.itemA]
   , position = Position 20 15
   , color = Color.rgb 0.8 0.8 0.6
  }
  (HouseholdCharacteristics { monthlyConsumptionA = 0 })


type alias HouseHoldGeneratorState =
    {   count : Int
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
      , households = []
      , maxHouseHolds = maxHouseHolds
      }


generateHouseholds : Int -> Int -> List Entity
generateHouseholds intSeed numberOfHouseholds =
  let
    i = initialHouseHoldGeneratorState intSeed config.numberOfHouseholds
    s = List.foldl newState i (List.range 0  (numberOfHouseholds - 1))
  in
    s.households


initialInventory =
    [config.itemA |> ModelTypes.setQuantity 1]

newState : Int -> HouseHoldGeneratorState -> HouseHoldGeneratorState
newState k s =
    let
         (i, seed1) = Random.step (Random.int 0 config.gridWidth) s.seed
         (j, seed2) = Random.step (Random.int 0 config.gridWidth) seed1
         newCount = s.count + 1
         newName = String.fromInt newCount -- thus each household has a unique identifier
         newHousehold = initialHousehold |> setPosition i j |> setName newName |> Entity.setInventory initialInventory
     in
       {s | count = newCount
          , seed = seed2
          , existingPositions = (Position i j) :: s.existingPositions
          , households = newHousehold :: s.households}


