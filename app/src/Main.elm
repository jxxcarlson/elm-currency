module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Background as Background
import Entity
import Html exposing (Html)
import Engine
import EngineData
import CellGrid.Render
import Money
import State exposing(State)
import Time
import Style
import Report
import String.Interpolate exposing(interpolate)



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
    , runState : RunState
    }

type RunState = Running | Paused | End

type Msg
    = NoOp
    | CellGrid CellGrid.Render.Msg
    | Tick Time.Posix
    | ToggleRun


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = "App started"
      , output = "App started"
      , counter = 0
      , state = (State.initialStateWithHouseholds 400 EngineData.config.maxHouseholds )
      , runState = Paused
      }
    , Cmd.none
    )


subscriptions model =
    Time.every EngineData.config.tickLoopInterval  Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CellGrid msg_ ->
            ( model, Cmd.none)

        Tick _ ->
            case model.runState of
                Running ->
                    ({model | counter = model.counter + 1
                      , state = Engine.nextState EngineData.config.cycleLength model.counter model.state }, Cmd.none)
                _ -> (model, Cmd.none)

        ToggleRun ->
            case model.runState of
                Paused ->
                    ({ model | runState = Running}, Cmd.none)
                Running ->
                    ({ model | runState = Paused}, Cmd.none)
                End ->
                  ({ model | runState = End}, Cmd.none)

--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [centerX] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
        column Style.mainColumn
          [ title "Simulator II"
            , row [centerX]
                [ lhColumn model
                , dashboard model
                ]
            ]



lhColumn : Model -> Element Msg
lhColumn model =
    column Style.lhColumn
            [ displayState model
            , footer model
            ]

dashboard : Model -> Element msg
dashboard model =
    column Style.dashboard [
       el [Font.family [Font.typeface "Courier"]] (text <| clock model.counter)
     , el [] (text <| "H = " ++ fiatHoldingsDisplay model)
     , el [] (text <| "Households A = " ++ String.fromInt (Report.inventoryOf "AA" model.state))
     ]

footer  model =
    row [paddingXY 10 0, Font.size 14, spacing 15, centerX, Background.color Style.lightColor, width (px (round EngineData.config.renderWidth)), height (px 50)] [
        runButton model, el [Font.family [Font.typeface "Courier"]] (text <| clock model.counter)
      ]

fiatHoldingsDisplay model =
    case Entity.fiatHoldingsOEntities model.counter model.state.households  of
        Just value ->  Money.valueToString value
        Nothing -> "--"


clock : Int -> String
clock k =
    let
        day = k |> (\x -> x + 1) |> String.fromInt |> String.padLeft 0 ' '
        month = k // 30 |> (\x -> x + 1) |> String.fromInt |> String.padLeft 2 '0'
        dayInMonth = modBy 30 k |> (\x -> x + 1) |> String.fromInt |> String.padLeft 2 '0'
    in
    interpolate "t = {0}: {1}/{2}" [day, month, dayInMonth]

displayState : Model -> Element Msg
displayState model =
    row [centerX ] [
       Engine.render
         model.state
         |> Element.html |> Element.map CellGrid
     ]

title : String -> Element msg
title str =
    row [ centerX, Font.bold ] [ text str ]


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
             Running -> "Running"
             Paused -> "Paused"
             End -> "End"
    in
    row [ ]
        [ Input.button Style.button
            { onPress = Just ToggleRun
            , label = el [ centerY ] (text label)
            }
        ]