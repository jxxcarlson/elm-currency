module Engine exposing (State, render, nextState, initialState, initialStateWithHouseholds)

import Entity exposing(Entity)
import EngineData
import Random
import Random.List
import Money
import Account
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
       0 -> payHouseholds t state |> dailyActivity t
       15 -> payHouseholds t state |> dailyActivity t
       _ ->  dailyActivity t state


dailyActivity : Int -> State -> State
dailyActivity t state =
    state


householdConsumptionStep : Int -> State -> State
householdConsumptionStep t state =
    state


nearestShop : Entity -> State -> Maybe Entity
nearestShop e state =
    let
        shops = getShops state
        distances = (List.map (Entity.distance e)) shops
        shopswithDistances = List.map2 Tuple.pair shops distances
        m = List.minimum distances |> Maybe.withDefault 0
        closestShops = List.filter (\(s, d) -> abs(d - m) < 0.001) shopswithDistances |> List.map Tuple.first
    in
        List.head closestShops

getShops : State -> List Entity
getShops state =
    List.filter (\e -> Entity.getType e == Entity.TShop) state.businesses


{-|
Select a household at random from those with the least
consumption of A.
-}
selectHouseholdWithLeastInventory : Int -> State -> (State, Maybe Entity)
selectHouseholdWithLeastInventory t state =
  let
      m = minimumHouseholdInventory state
      candidates = List.filter (\h -> Entity.inventorySize h == m) state.households
      (result, newSeed) = Random.step (Random.List.choose candidates) state.seed
  in
    ({state | seed = newSeed }, Tuple.first result)




minimumHouseholdInventory : State -> Int
minimumHouseholdInventory state =
    state.households
      |> List.map Entity.inventorySize
      |> List.minimum
      |> Maybe.withDefault 0




payHouseholds : Int -> State -> State
payHouseholds t state =
    let
        households = creditHouseHolds t EngineData.config.monthlyFiatIncome state.households
     in
        {state | households = households}


creditHouseHolds : Int -> Float -> List Entity -> List Entity
creditHouseHolds t value households =
   List.map (creditHousehold t value) households


creditHousehold : Int -> Float -> Entity -> Entity
creditHousehold t value entity =
    let
        currency = Money.createFiatCurrency EngineData.config.fiatCurrencyName
        money = Money.createInfinite currency 0 EngineData.config.monthlyFiatIncome
        account = Account.credit (Money.bankTime t) money (Entity.getFiatAccount entity)
    in
        Entity.setFiatAccount account entity


