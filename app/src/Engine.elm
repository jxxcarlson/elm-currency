module Engine exposing (render, nextState)

import Action
import Entity exposing(Entity)
import EngineData
import Random
import State exposing(State, initialState)
import Html exposing (Html)
import Color exposing(Color)
import CellGrid exposing(CellGrid, Dimensions)
import CellGrid.Render exposing (CellStyle)



config =
    { maxRandInt = 100000}



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


nextState : Int -> Int -> State -> State
nextState period t state =
    case (modBy period t) of
       0 -> Action.payHouseholds t state |>  Action.dailyActivity t
       15 ->  Action.payHouseholds t state |> Action.dailyActivity t
       _ ->   Action.dailyActivity t state

