module Attrs exposing (EachSide, errorField, grayButton, greenButton, inputField)

import Colors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


type alias EachSide =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


greenButton : List (Attribute msg)
greenButton =
    [ Background.color green
    , Border.color darkGreen
    , Border.rounded 3
    , Border.widthEach <| EachSide 0 0 3 0
    , Font.bold
    , Font.color white
    , paddingXY 20 6
    ]


grayButton : List (Attribute msg)
grayButton =
    [ Background.color lightGrey
    , Border.color grey
    , Border.rounded 3
    , Border.widthEach <| EachSide 0 1 1 0
    , Font.bold
    , Font.color darkCharcoal
    ]


inputField : List (Attribute msg)
inputField =
    [ Border.width 1
    , Border.rounded 3
    , Border.color lightCharcoal
    , padding 3
    ]


errorField : List (Attribute msg)
errorField =
    [ Font.color red ]
