module Engine exposing (State, render, initialState, initialStateWithHouseholds)

import Entity exposing(Entity)
import EngineData
import Random
import Html exposing (Html)
import Color exposing(Color)
import CellGrid exposing(CellGrid, Dimensions)
import CellGrid.Render exposing (CellStyle)

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

initialStateWithHouseholds : Int -> Int -> State
initialStateWithHouseholds intSeed numberOfHouseholds =
   let
       s = initialState intSeed
   in
       {s | households = EngineData.generateHouseholds intSeed numberOfHouseholds}

render : State -> Html CellGrid.Render.Msg
render s =
    CellGrid.Render.asHtml { width = 500, height = 500} cellStyle (toCellGrid s)



toCellGrid : State -> CellGrid Color
toCellGrid s =
    let
        gridWidth = EngineData.config.gridWidth
        initialGrid  : CellGrid Color
        initialGrid = CellGrid.initialize (Dimensions gridWidth gridWidth) (\i j -> Color.black)

        setCell : Entity -> CellGrid Color -> CellGrid Color
        setCell e grid = CellGrid.set (Entity.getPosition  e) (Entity.getColor e) grid
    in
        List.foldl setCell initialGrid (s.businesses ++ s.households)




cellStyle : CellStyle Color
cellStyle =
    { toColor = identity
    , cellWidth = 10
     , cellHeight = 10
    , gridLineColor = Color.rgb 0 0 0.8
    , gridLineWidth = 0.5
    }

nextState : Int -> State -> State
nextState t state =
    let
        (newRandInt, newSeed) = Random.step (Random.int 0 config.maxRandInt) state.seed
    in
    {state | randInt = newRandInt, seed = newSeed}