module Style exposing
    ( button
    , dashboard
    , lightColor
    , log
    , mainColumn
    , titleColor
    )

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input


mainColumn =
    [ Background.color (rgb255 100 100 120)
    , spacing 20
    , width fill
    , height fill
    ]


dashboard =
    [ Background.color (rgb255 200 200 200)
    , paddingXY 12 18
    , width (px 300)
    , height (px 570)
    , spacing 6
    , Font.size 14
    , Font.family [ Font.typeface "Courier" ]
    ]


log =
    [ Background.color (rgb255 200 200 200)
    , paddingXY 12 18
    , width (px 300)
    , height (px 570)
    , spacing 3
    , Font.size 14
    , Font.family [ Font.typeface "Courier" ]
    ]


button =
    let
        g =
            80
    in
    [ Background.color (rgb255 g g g)
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    ]


lightColor : Color
lightColor =
    rgb255 200 200 200


titleColor : Color
titleColor =
    rgb255 190 190 255
