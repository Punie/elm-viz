module Main exposing (main)

import Browser
import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import LineChart
import LineChart.Colors as Colors
import LineChart.Dots as Dots


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Point =
    { x : Float, y : Float }


type alias DataPoint =
    { rate : Float
    , year : Float
    , kind : String
    , city : String
    }


type alias FormattedData =
    { fnb : List DataPoint
    , toem : List DataPoint
    , tfb : List DataPoint
    , th : List DataPoint
    }


type alias Model =
    { first : Result Json.Decode.Error (List Point)
    , second : Result Json.Decode.Error (List Point)
    , data : Maybe FormattedData
    , errors : List Http.Error
    }


type Msg
    = GotData (Result Http.Error (List DataPoint))


init : () -> ( Model, Cmd Msg )
init _ =
    let
        first =
            Json.Decode.decodeString pointsDecoder mockJSON1

        second =
            Json.Decode.decodeString pointsDecoder mockJSON2

        request =
            Http.get
                { url = "http://entrepot.metropolegrenoble.fr/opendata/Imposition/imposition.json"
                , expect = Http.expectJson GotData dataPointsDecoder
                }
    in
    ( Model first second Nothing [], request )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData (Ok data) ->
            let
                filtered =
                    List.filter (\{ city } -> city == "Grenoble") data

                formatted =
                    { fnb = List.filter (\{ kind } -> String.contains "FNB" kind) filtered
                    , toem = List.filter (\{ kind } -> String.contains "TOEM" kind) filtered
                    , tfb = List.filter (\{ kind } -> String.contains "TFB" kind) filtered
                    , th = List.filter (\{ kind } -> String.contains "TH" kind) filtered
                    }
            in
            ( { model | data = Just formatted }, Cmd.none )

        GotData (Err err) ->
            ( { model | errors = err :: model.errors }, Cmd.none )


viewData : Maybe FormattedData -> List Http.Error -> Html Msg
viewData mdata errors =
    case mdata of
        Just data ->
            LineChart.view .year
                .rate
                [ LineChart.line Colors.blueLight Dots.circle "FNB" data.fnb
                , LineChart.line Colors.redLight Dots.circle "TFB" data.tfb
                , LineChart.line Colors.greenLight Dots.circle "TH" data.th
                , LineChart.line Colors.purpleLight Dots.circle "TOEM" data.toem
                ]

        Nothing ->
            let
                errorToString error =
                    case error of
                        Http.BadUrl url ->
                            "Bad URL: " ++ url

                        Http.Timeout ->
                            "Request timeout."

                        Http.NetworkError ->
                            "Network error."

                        Http.BadStatus n ->
                            "Bad status: " ++ String.fromInt n

                        Http.BadBody body ->
                            "Bad body: " ++ body
            in
            errors
                |> List.map errorToString
                |> List.foldr (++) ""
                |> Html.text


viewMocks : Model -> Html Msg
viewMocks model =
    case ( model.first, model.second ) of
        ( Ok data1, Ok data2 ) ->
            LineChart.view2 .x .y data1 data2

        ( Ok data, Err err ) ->
            Html.div []
                [ LineChart.view1 .x .y data
                , Html.text <| "mockJSON2: " ++ Json.Decode.errorToString err
                ]

        ( Err err, Ok data ) ->
            Html.div []
                [ LineChart.view1 .x .y data
                , Html.text <| "mockJSON1: " ++ Json.Decode.errorToString err
                ]

        _ ->
            Html.text ""


view : Model -> Html Msg
view model =
    Html.div []
        [ viewMocks model
        , viewData model.data model.errors
        ]


pointDecoder : Decoder Point
pointDecoder =
    Json.Decode.succeed Point
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float


pointsDecoder : Decoder (List Point)
pointsDecoder =
    Json.Decode.at [ "data" ] <|
        Json.Decode.list pointDecoder


dataPointDecoder : Decoder DataPoint
dataPointDecoder =
    let
        year : String -> Decoder Float
        year str =
            case String.toFloat str of
                Nothing ->
                    Json.Decode.fail "Not a valid INT"

                Just n ->
                    Json.Decode.succeed n
    in
    Json.Decode.succeed DataPoint
        |> required "taux" Json.Decode.float
        |> required "annee" (Json.Decode.string |> Json.Decode.andThen year)
        |> required "type" Json.Decode.string
        |> required "Commune" Json.Decode.string


dataPointsDecoder : Decoder (List DataPoint)
dataPointsDecoder =
    Json.Decode.at [ "data" ] <|
        Json.Decode.list dataPointDecoder


mockJSON1 =
    """
{
    "data": [
        { "x": 0, "y": 2 },
        { "x": 5, "y": 5 },
        { "x": 10, "y": 10 }
    ]
}
"""


mockJSON2 =
    """
{
    "data": [
        { "x": 0, "y": 0 },
        { "x": 5, "y": 5 },
        { "x": 10, "y": 8 }
    ]
}
"""
