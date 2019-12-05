module State exposing (State, initialState, initialStateWithHouseholds)

import Entity exposing(Entity)
import Random
import EngineData

type alias State =
  { businesses : List Entity
  , households : List Entity
  , seed : Random.Seed
  , randInt : Int
  }

initialState : Int -> State
initialState k =
    { businesses = []
     , households = []
     , seed = Random.initialSeed 1234
     , randInt = 4321
     }


initialStateWithHouseholds : Int -> Int -> State
initialStateWithHouseholds intSeed numberOfHouseholds =
   let
       s = initialState intSeed
   in
       {s | households = EngineData.generateHouseholds intSeed numberOfHouseholds,  businesses = EngineData.businesses}
