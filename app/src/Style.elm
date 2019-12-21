module Style exposing (..)


import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input

mainColumn =
    [   Background.color (rgb255  100 100 120)
    , paddingXY 0 30
    , spacing 0
    , width fill
    , height fill

    ]

lhColumn =
    [ Background.color (rgb255  100 100 120)
    , paddingXY 20 60
    , spacing 20
    ]


dashboard =
    [  Background.color (rgb255  200 200 200)
    , paddingXY 12 8
    , width (px 300)
    , height (px 570)
    , spacing 10
    , Font.size 14
    , Font.family [Font.typeface "Courier"]
    ]


button =
  let
    g = 80
  in
    [ Background.color (rgb255 g g g)
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    ]

lightColor : Color
lightColor = rgb255 200 200 200