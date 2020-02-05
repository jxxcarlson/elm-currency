module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import CellGrid.Render
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Engine
import EngineData
import Entity
import Html exposing (Html)
import Money
import Report
import State exposing (State)
import String.Interpolate exposing (interpolate)
import Style
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , output : String
    , counter : Int
    , state : State
    , configurationIndex : Int
    , configuration : EngineData.Config
    , runState : RunState
    }


type RunState
    = Running
    | Paused
    | End


type Msg
    = NoOp
    | CellGrid CellGrid.Render.Msg
    | Tick Time.Posix
    | ToggleRun
    | Reset


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        config =
            EngineData.getConfiguration 0
    in
    ( { input = "App started"
      , output = "App started"
      , counter = 0
      , configurationIndex = 0
      , configuration = config
      , state = State.configure config 400
      , runState = Paused
      }
    , Cmd.none
    )


subscriptions model =
    Time.every model.configuration.tickLoopInterval Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CellGrid msg_ ->
            ( model, Cmd.none )

        Tick _ ->
            case model.runState of
                Running ->
                    let
                        ( counter, runState ) =
                            if model.counter >= model.state.config.cycleLength then
                                ( model.counter, End )

                            else
                                ( model.counter + 1, Running )
                    in
                    ( { model
                        | counter = counter
                        , state = Engine.nextState model.configuration counter model.state
                        , runState = runState
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ToggleRun ->
            case model.runState of
                Paused ->
                    ( { model | runState = Running }, Cmd.none )

                Running ->
                    ( { model | runState = Paused }, Cmd.none )

                End ->
                    ( { model | runState = End }, Cmd.none )

        Reset ->
            let
                config =
                    model.configuration
            in
            ( { model
                | state = State.configure config 400
                , runState = Paused
                , counter = 0
              }
            , Cmd.none
            )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [ centerX ] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column Style.mainColumn
        [ title "Simulator II"
        , row [ centerX, spacing 10 ]
            [ displayState model
            , displayLog model.state
            , dashboard model
            ]
        , footer model
        ]


dashboard : Model -> Element msg
dashboard model =
    column Style.dashboard
        [ el [] (text <| model.state.config.title)
        , el [] (text <| "------------------------------")
        , el [] (text <| "Cycle length = " ++ String.fromInt model.state.config.cycleLength)
        , el [] (text <| clock model.counter)
        , el [] (text <| "------------------------------")
        , el [] (text <| "Households = " ++ String.fromInt (model.state.households |> List.length))
        , el [] (text <| "Household acct bal = " ++ fiatHoldingsDisplay model)
        , el [] (text <| "Household inventory = " ++ String.fromInt (Report.householdInventoryOf "AA" model.state))
        , el [] (text <| "[min, max] = " ++ (List.map String.fromInt (Report.minMaxHouseholdInventoryOf "AA" model.state) |> String.join ", "))
        , el [] (text <| "Total purchases = " ++ String.fromInt model.state.totalHouseholdPurchases)
        , el [] (text <| "Total consumed = " ++ String.fromInt model.state.totalHouseholdConsumption)
        , el [] (text <| "Net purchases = " ++ String.fromInt (model.state.totalHouseholdPurchases - model.state.totalHouseholdConsumption))
        , el [] (text <| "------------------------------")
        , el [] (text <| "Business inventories = " ++ businessInventory model)
        , el [] (text <| "Fiat balances = " ++ fiatBalances model)
        , el [] (text <| "CC balances = " ++ ccBalances model)
        ]


displayLog : State -> Element msg
displayLog state =
    let
        displayItem : String -> Element msg
        displayItem str =
            el [] (text str)

        log =
            "Log" :: "--------------------------" :: List.take 30 state.log
    in
    List.map displayItem log
        |> column Style.log


businessInventory : Model -> String
businessInventory model =
    List.map String.fromInt (Report.businessInventoryOf "AA" model.state)
        |> String.join ", "


fiatBalances : Model -> String
fiatBalances model =
    List.map String.fromFloat (Report.fiatBalanceOf (Money.bankTime model.state.tick) model.state.businesses)
        |> String.join ", "


ccBalances : Model -> String
ccBalances model =
    List.map String.fromFloat (Report.ccBalanceOf (Money.bankTime model.state.tick) model.state.businesses)
        |> String.join ", "


footer model =
    row [ alignBottom, paddingXY 10 0, Font.size 14, spacing 15, centerX, Background.color Style.lightColor, width fill, height (px 40) ]
        [ resetButton model
        , runButton model
        , el [ Font.family [ Font.typeface "Courier" ] ] (text <| clock model.counter)
        ]


fiatHoldingsDisplay model =
    case Entity.fiatHoldingsOEntities model.counter model.state.households of
        Just value ->
            Money.valueToString value

        Nothing ->
            "--"


clock : Int -> String
clock k =
    let
        day =
            k |> String.fromInt |> String.padLeft 0 ' '

        month =
            k // 30 |> (\x -> x + 1) |> String.fromInt |> String.padLeft 2 '0'

        dayInMonth =
            modBy 30 k |> String.fromInt |> String.padLeft 2 '0'
    in
    interpolate "t = {0}: {1}/{2}" [ day, dayInMonth, month ]


displayState : Model -> Element Msg
displayState model =
    row [ centerX, spacing 10 ]
        [ Engine.render
            model.configuration
            model.state
            |> Element.html
            |> Element.map CellGrid
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold, paddingEach { top = 40, bottom = 0, left = 0, right = 0 } ] [ text str ]


outputDisplay : Model -> Element msg
outputDisplay model =
    row [ centerX ]
        [ text model.output ]



{- Buttons -}


runButton : Model -> Element Msg
runButton model =
    let
        label =
            case model.runState of
                Running ->
                    "Running"

                Paused ->
                    "Run"

                End ->
                    "End"
    in
    row []
        [ Input.button Style.button
            { onPress = Just ToggleRun
            , label = el [ centerY ] (text label)
            }
        ]


resetButton model =
    row []
        [ Input.button Style.button
            { onPress = Just Reset
            , label = el [ centerY ] (text "Reset")
            }
        ]
