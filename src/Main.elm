port module Main exposing (main)

import Browser
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (optional, required)
import LineChart
import LineChart.Colors as Colors
import LineChart.Dots as Dots
import RemoteData exposing (RemoteData(..), WebData)
import Set


port results : (Value -> msg) -> Sub msg


port parseCSV : String -> Cmd msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Coordinates =
    { x : Float, y : Float }


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


type alias Astronaut =
    { name : String
    , hoursInSpace : Maybe Int
    , gender : Gender
    , selectionYear : Int
    , status : Status
    , category : Category
    }


type Gender
    = Male
    | Female


type Status
    = Former
    | Deceased
    | Current
    | Management
    | Candidate


type Category
    = Military
    | Civilian
    | Unknown


type alias Model =
    { coord1 : Result Json.Decode.Error (List Coordinates)
    , coord2 : Result Json.Decode.Error (List Coordinates)
    , taxes : WebData (List Tax)
    , cities : List String
    , selectedCity : Maybe String
    , csvValue : Maybe (List Astronaut)
    }


type Msg
    = GotData (Result Http.Error (List Tax))
    | SelectCity String
    | GotMoreData (Result Http.Error String)
    | Parsed Value


init : () -> ( Model, Cmd Msg )
init _ =
    let
        coord1 =
            Json.Decode.decodeString pointsDecoder mockJSON1

        coord2 =
            Json.Decode.decodeString pointsDecoder mockJSON2

        request1 =
            Http.get
                { url = "http://entrepot.metropolegrenoble.fr/opendata/Imposition/imposition.json"
                , expect = Http.expectJson GotData dataPointsDecoder
                }

        request2 =
            Http.get
                { url = "https://pastebin.com/raw/ec5VV21w"
                , expect = Http.expectString GotMoreData
                }
    in
    ( Model coord1 coord2 Loading [] Nothing Nothing
    , Cmd.batch
        [ request1
        , request2
        ]
    )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData (Ok data) ->
            let
                cities =
                    data
                        |> List.map .city
                        |> Set.fromList
                        |> Set.toList
            in
            ( { model
                | taxes = Success data
                , cities = cities
              }
            , Cmd.none
            )

        GotData (Err err) ->
            ( { model
                | taxes = Failure err
              }
            , Cmd.none
            )

        SelectCity city ->
            ( { model
                | selectedCity = Just city
              }
            , Cmd.none
            )

        GotMoreData (Ok data) ->
            ( model, parseCSV data )

        GotMoreData (Err _) ->
            ( model, Cmd.none )

        Parsed value ->
            let
                parsed =
                    Json.Decode.decodeValue astronautsDecoder value

                _ =
                    Debug.log "decoder" parsed
            in
            ( { model | csvValue = Result.toMaybe parsed }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    results Parsed


viewMocks : Model -> Html Msg
viewMocks model =
    case ( model.coord1, model.coord2 ) of
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


viewTaxes : WebData Taxes -> Html Msg
viewTaxes rmdata =
    case rmdata of
        Success data ->
            LineChart.view .year
                .rate
                [ LineChart.line Colors.blueLight Dots.circle "FNB" data.fnb
                , LineChart.line Colors.redLight Dots.circle "TFB" data.tfb
                , LineChart.line Colors.greenLight Dots.circle "TH" data.th
                , LineChart.line Colors.purpleLight Dots.circle "TOEM" data.toem
                ]

        Failure err ->
            let
                errorToString error =
                    case error of
                        Http.BadUrl url ->
                            "Bad URL: " ++ url

                        Http.Timeout ->
                            "Request timed out"

                        Http.NetworkError ->
                            "Internet connection was lost"

                        Http.BadStatus n ->
                            "Response contained a bad status: " ++ String.fromInt n

                        Http.BadBody body ->
                            "Could not decode payload: " ++ body
            in
            Html.text <| errorToString err

        Loading ->
            Html.text "Loading..."

        NotAsked ->
            Html.text ""


view : Model -> Html Msg
view model =
    Html.div []
        [ viewMocks model
        , Html.select [ Html.Attributes.value (Maybe.withDefault "" model.selectedCity), onChange SelectCity ]
            (List.map (\city -> Html.option [ Html.Attributes.value city ] [ Html.text city ]) ("" :: model.cities))
        , viewTaxes (RemoteData.map (toFiltered model.selectedCity) model.taxes)
        ]


onChange : (String -> msg) -> Attribute msg
onChange handler =
    Html.Events.on "change" <| Json.Decode.map handler (Json.Decode.at [ "target", "value" ] Json.Decode.string)


pointDecoder : Decoder Coordinates
pointDecoder =
    Json.Decode.succeed Coordinates
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float


pointsDecoder : Decoder (List Coordinates)
pointsDecoder =
    Json.Decode.at [ "data" ] <|
        Json.Decode.list pointDecoder


dataPointDecoder : Decoder Tax
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
    Json.Decode.succeed Tax
        |> required "taux" Json.Decode.float
        |> required "annee" (Json.Decode.string |> Json.Decode.andThen year)
        |> required "type" Json.Decode.string
        |> required "Commune" Json.Decode.string


dataPointsDecoder : Decoder (List Tax)
dataPointsDecoder =
    Json.Decode.at [ "data" ] <|
        Json.Decode.list dataPointDecoder


astronautDecoder : Decoder Astronaut
astronautDecoder =
    let
        gender : String -> Decoder Gender
        gender str =
            case str of
                "Male" ->
                    Json.Decode.succeed Male

                "Female" ->
                    Json.Decode.succeed Female

                _ ->
                    Json.Decode.fail "Not a gender"

        status : String -> Decoder Status
        status str =
            case str of
                "Former" ->
                    Json.Decode.succeed Former

                "Deceased" ->
                    Json.Decode.succeed Deceased

                "Current" ->
                    Json.Decode.succeed Current

                "Management" ->
                    Json.Decode.succeed Management

                "Candidate" ->
                    Json.Decode.succeed Candidate

                _ ->
                    Json.Decode.fail "Not a status"

        category : String -> Decoder Category
        category str =
            case str of
                "Military" ->
                    Json.Decode.succeed Military

                "Civilian" ->
                    Json.Decode.succeed Civilian

                _ ->
                    Json.Decode.fail "Not a category"
    in
    Json.Decode.succeed Astronaut
        |> required "astronaut" Json.Decode.string
        |> required "cumulativehoursofspaceflighttime" (Json.Decode.nullable Json.Decode.int)
        |> required "gender" (Json.Decode.string |> Json.Decode.andThen gender)
        |> required "selectionyear" Json.Decode.int
        |> required "status" (Json.Decode.string |> Json.Decode.andThen status)
        |> optional "militaryorcivilian" (Json.Decode.string |> Json.Decode.andThen category) Unknown


astronautsDecoder : Decoder (List Astronaut)
astronautsDecoder =
    Json.Decode.list astronautDecoder


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
