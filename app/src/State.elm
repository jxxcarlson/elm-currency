module State exposing (State, initialState, initialStateWithHouseholds, initialStateWithHouseholdsAndSeed)

import Entity exposing(Entity)
import Random
import EngineData

type alias State =
  {  suppliers : List Entity
   , businesses : List Entity
   , households : List Entity
   , seed : Random.Seed
   , randInt : Int
   , config : EngineData.Config
   , totalHouseholdPurchases : Int
   , totalHouseholdConsumption : Int
  }

initialState : Int -> State
initialState k =
    { suppliers = []
     , businesses = []
     , households = []
     , seed = Random.initialSeed 1234
     , randInt = 4321
     , config = EngineData.config
     , totalHouseholdPurchases = 0
     , totalHouseholdConsumption = 0
     }


initialStateWithHouseholds : Int -> Int -> State
initialStateWithHouseholds intSeed numberOfHouseholds =
   let
       s = initialState intSeed
   in
       {s |   households = EngineData.generateHouseholds intSeed numberOfHouseholds
           ,  businesses = EngineData.businesses
           ,  suppliers = EngineData.suppliers }


initialStateWithHouseholdsAndSeed : Random.Seed -> Int -> State
initialStateWithHouseholdsAndSeed seed numberOfHouseholds =
   let
       s = initialState 0
       (i, newSeed) = Random.step (Random.int 0 100000) seed
   in
       {s |   households = EngineData.generateHouseholds i numberOfHouseholds
           ,  businesses = EngineData.businesses
           ,  suppliers = EngineData.suppliers
           , seed = newSeed}
