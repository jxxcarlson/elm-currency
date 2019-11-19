module Engine exposing (State)

import Entity exposing(Entity)
import EngineData
import Random

type alias State =
  { businesses : List Entity
  , households : List Entity
  , seed : Random.Seed
  , randInt : Int
  }


config =
    { maxRandInt = 100000}

initialState : Int -> State
initialState k =
    { businesses = EngineData.businesses
     , households = []
     , seed = Random.initialSeed 1234
     , randInt = 4321
     }

nextState : Int -> State -> State
nextState t state =
    let
        (newRandInt, newSeed) = Random.step (Random.int 0 config.maxRandInt) state.seed
    in
    {state | randInt = newRandInt, seed = newSeed}
