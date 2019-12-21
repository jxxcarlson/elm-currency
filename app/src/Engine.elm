module Engine exposing (render, nextState)

import Action
import Entity exposing(Entity)
import EngineData
import Random
import State exposing(State, initialState)
import Html exposing (Html)
import Color exposing(Color)
import CellGrid exposing(CellGrid, Dimensions)
import CellGrid.Canvas
import CellGrid.Render exposing (CellStyle)
import Utility



config =
    { maxRandInt = 100000}



render : State -> Html CellGrid.Render.Msg
render s =
    CellGrid.Render.asHtml { width = 500, height = 500} cellStyle (toCellGrid s)

--render2 : State -> Html CellGrid.Canvas.Msg
--render2 s =
--    CellGrid.Canvas.asHtml { width = 580, height = 580} cellStyle s.organisms
--
--
--
--cellStyle : CellStyle Entity
--cellStyle =
--    {  toColor = (\e -> Entity.getColor e)
--     , toRadius = (\e -> 5 )
--     , toPosition = (\o -> Organism.position o)
--     , cellWidth = EngineData.config.renderWidth / (toFloat EngineData.config.gridWidth)
--     , cellHeight = EngineData.config.renderWidth / (toFloat EngineData.config.gridWidth)
--    }


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
    , cellWidth = EngineData.config.renderWidth / (toFloat EngineData.config.gridWidth)
     , cellHeight = EngineData.config.renderWidth / (toFloat EngineData.config.gridWidth)
    , gridLineColor = Color.rgb 0 0 0.8
    , gridLineWidth = 0.5
    }

newRandomNumber : State -> State
newRandomNumber state =
    let
        (newRandInt, newSeed) = Random.step (Random.int 0 config.maxRandInt) state.seed
    in
    {state | randInt = newRandInt, seed = newSeed}


nextState :  Int -> State -> State
nextState t state =
    state
      |> Action.businessBuyGoods
      |> Action.payHouseholds t
      |> Utility.iterate 2 (Action.householdBuyGoods t)
      |> Action.consumeA t

