module Point exposing (Point, decoder, mock1, mock2, point, viewBoth, viewError, viewOne)

import Html exposing (Html)
import Json.Decode exposing (Decoder, Error, at, errorToString, float, list, succeed)
import Json.Decode.Pipeline exposing (required)
import LineChart


type alias Point =
    { x : Float
    , y : Float
    }


viewOne : List Point -> Html msg
viewOne data =
    LineChart.view1 .x .y data


viewBoth : List Point -> List Point -> Html msg
viewBoth data1 data2 =
    LineChart.view2 .x .y data1 data2


viewError : Error -> Html msg
viewError error =
    Html.text (errorToString error)


point : Decoder Point
point =
    succeed Point
        |> required "x" float
        |> required "y" float


decoder : Decoder (List Point)
decoder =
    at [ "data" ] <|
        list point


mock1 : String
mock1 =
    """
{
    "data": [
        { "x": 0, "y": 2 },
        { "x": 5, "y": 5 },
        { "x": 10, "y": 10 }
    ]
}
"""


mock2 : String
mock2 =
    """
{
    "data": [
        { "x": 0, "y": 0 },
        { "x": 5, "y": 5 },
        { "x": 10, "y": 8 }
    ]
}
"""
