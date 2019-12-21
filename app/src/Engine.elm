module Engine exposing (render, nextState)

import Action
import Entity exposing(Entity, TEntity(..))
import EngineData
import Random
import State exposing(State, initialState)
import Html exposing (Html)
import Color exposing(Color)
import CellGrid exposing(CellGrid, Dimensions)
import CellGrid.Canvas exposing (CellStyle)
import Utility



config =
    { maxRandInt = 100000}



render : State -> Html CellGrid.Canvas.Msg
render s =
    CellGrid.Canvas.asHtml { width = 500, height = 500} cellStyle (s.households ++ s.businesses)

entityRadius : Entity -> Float
entityRadius e =
    case Entity.getType e of
        THousehold -> 1.5 * (toFloat (1 + Entity.inventoryAmount "AA" e))
        TShop -> 0.5 * (toFloat (1 + Entity.inventoryAmount "AA" e))
        _ -> 0



cellStyle : CellStyle Entity
cellStyle =
    {  toColor = (\e -> Entity.getColor e)
     , toRadius = entityRadius
     , toPosition = (\e -> Entity.getPosition e)
     , cellWidth = EngineData.config.renderWidth / (toFloat EngineData.config.gridWidth)
     , cellHeight = EngineData.config.renderWidth / (toFloat EngineData.config.gridWidth)
    }


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

