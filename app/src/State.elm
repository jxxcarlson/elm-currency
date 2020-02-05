module State exposing (State, initialState, initialStateWithHouseholds, initialStateWithHouseholdsAndSeed)

import EngineData exposing (Config)
import Entity exposing (Entity)
import Random


type alias State =
    { suppliers : List Entity
    , businesses : List Entity
    , households : List Entity
    , seed : Random.Seed
    , randInt : Int
    , config : EngineData.Config
    , totalHouseholdPurchases : Int
    , totalHouseholdConsumption : Int
    , log : List String
    }


initialState : Int -> State
initialState k =
    { suppliers = []
    , businesses = []
    , households = []
    , seed = Random.initialSeed 1234
    , randInt = 4321
    , config = EngineData.getConfiguration 0
    , totalHouseholdPurchases = 0
    , totalHouseholdConsumption = 0
    , log = []
    }


initialStateWithHouseholds : Config -> Int -> Int -> State
initialStateWithHouseholds config intSeed numberOfHouseholds =
    let
        s =
            initialState intSeed
    in
    { s
        | households = EngineData.generateHouseholds config intSeed numberOfHouseholds
        , businesses = EngineData.businesses config
        , suppliers = EngineData.suppliers config
    }


initialStateWithHouseholdsAndSeed : Config -> Random.Seed -> Int -> State
initialStateWithHouseholdsAndSeed config seed numberOfHouseholds =
    let
        s =
            initialState 0

        ( i, newSeed ) =
            Random.step (Random.int 0 100000) seed
    in
    { s
        | households = EngineData.generateHouseholds config i numberOfHouseholds
        , businesses = EngineData.businesses config
        , suppliers = EngineData.suppliers config
        , seed = newSeed
    }
