module Tax exposing (Tax, Taxes, cities, decoder, tax, view)

import Html exposing (Html)
import Json.Decode exposing (Decoder, at, float, list, succeed)
import Json.Decode.Pipeline exposing (required)
import LineChart
import LineChart.Colors as Colors
import LineChart.Dots as Dots
import Set


type alias Tax =
    { rate : Float
    , year : Float
    , kind : String
    , city : String
    }


type alias Taxes =
    { fnb : List Tax
    , toem : List Tax
    , tfb : List Tax
    , th : List Tax
    }


view : Maybe String -> List Tax -> Html msg
view city rawdata =
    let
        data =
            toFiltered city rawdata
    in
    LineChart.view .year
        .rate
        [ LineChart.line Colors.blueLight Dots.circle "FNB" data.fnb
        , LineChart.line Colors.redLight Dots.circle "TFB" data.tfb
        , LineChart.line Colors.greenLight Dots.circle "TH" data.th
        , LineChart.line Colors.purpleLight Dots.circle "TOEM" data.toem
        ]


cities : List Tax -> List String
cities data =
    data
        |> List.map .city
        |> Set.fromList
        |> Set.toList


tax : Decoder Tax
tax =
    let
        year : String -> Decoder Float
        year str =
            case String.toFloat str of
                Just n ->
                    Json.Decode.succeed n

                Nothing ->
                    Json.Decode.fail "Not a valid INT"
    in
    Json.Decode.succeed Tax
        |> required "taux" Json.Decode.float
        |> required "annee" (Json.Decode.string |> Json.Decode.andThen year)
        |> required "type" Json.Decode.string
        |> required "Commune" Json.Decode.string


decoder : Decoder (List Tax)
decoder =
    Json.Decode.at [ "data" ] <|
        Json.Decode.list tax



-- Helpers


toFiltered : Maybe String -> List Tax -> Taxes
toFiltered mselectedCity taxes =
    case mselectedCity of
        Just selectedCity ->
            let
                filtered =
                    List.filter (\{ city } -> city == selectedCity) taxes
            in
            { fnb = List.filter (\{ kind } -> String.contains "FNB" kind) filtered
            , toem = List.filter (\{ kind } -> String.contains "TOEM" kind) filtered
            , tfb = List.filter (\{ kind } -> String.contains "TFB" kind) filtered
            , th = List.filter (\{ kind } -> String.contains "TH" kind) filtered
            }

        Nothing ->
            { fnb = []
            , toem = []
            , tfb = []
            , th = []
            }
