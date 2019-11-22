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
import Time
import Style
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
    , state : Engine.State
    }


type Msg
    = NoOp
    | InputText String
    | ReverseText
    | CellGrid CellGrid.Render.Msg
    | Tick Time.Posix


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = "App started"
      , output = "App started"
      , counter = 0
      , state = (Engine.initialStateWithHouseholds 400 EngineData.config.maxHouseholds )
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

        InputText str ->
            ( { model | input = str, output = str }, Cmd.none )

        ReverseText ->
            ( { model | output = model.output |> String.reverse |> String.toLower }, Cmd.none )

        CellGrid msg_ ->
            ( model, Cmd.none)

        Tick _ ->
            ({model | counter = model.counter + 1
              , state = Engine.nextState EngineData.config.cycleLength model.counter model.state }, Cmd.none)

--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column Style.mainColumn
            [ title "Simulator II"
            , displayState model
            , displayDashboard model
            --, inputText model
           -- , appButton
            --, outputDisplay model
            ]

displayDashboard  model =
    row [Font.size 14, spacing 15, centerX, Background.color Style.lightColor, width (px (round EngineData.config.renderWidth)), height (px 30)] [
      el [Font.family [Font.typeface "Courier"]] (text <| clock model.counter)
      , el [] (text <| fiatHoldingsDisplay model)
      ]

fiatHoldingsDisplay model =
    case Entity.fiatHoldingsOEntities model.counter model.state.households  of
        Just value ->  Money.valueToString value
        Nothing -> "--"


clock : Int -> String
clock k =
    let
        day = k |> (\x -> x + 1) |> String.fromInt |> String.padLeft 3 ' '
        month = k // 30 |> (\x -> x + 1) |> String.fromInt |> String.padLeft 2 '0'
        dayInMonth = modBy 30 k |> (\x -> x + 1) |> String.fromInt |> String.padLeft 2 '0'
    in
    interpolate "{0}: {1}/{2}" [day, month, dayInMonth]

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


inputText : Model -> Element Msg
inputText model =
    Input.text []
        { onChange = InputText
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [] (text "")
        }


appButton : Element Msg
appButton =
    row [ centerX ]
        [ Input.button Style.button
            { onPress = Just ReverseText
            , label = el [ centerX, centerY ] (text "Reverse")
            }
        ]



--
-- STYLE
--
