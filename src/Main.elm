module Main exposing (main)

import Astronaut exposing (Astronaut, Category(..), Gender(..), Status(..))
import Browser
import File exposing (File)
import File.Select
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode exposing (Value)
import Point exposing (Point)
import RemoteData exposing (RemoteData(..), WebData)
import Task
import Tax exposing (Tax)
import Utils


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { points1 : Result Json.Decode.Error (List Point)
    , points2 : Result Json.Decode.Error (List Point)
    , taxes : WebData (List Tax)
    , cities : List String
    , selectedCity : Maybe String
    , csvValue : Maybe (List Astronaut)
    , file : Maybe File
    }


type Msg
    = GotData (Result Http.Error (List Tax))
    | SelectCity String
    | GotMoreData (Result Http.Error String)
    | Parsed Value
    | SelectFile
    | FileSelected File
    | FileLoaded String


init : () -> ( Model, Cmd Msg )
init _ =
    let
        coord1 =
            Json.Decode.decodeString Point.decoder Point.mock1

        coord2 =
            Json.Decode.decodeString Point.decoder Point.mock2

        request1 =
            Http.get
                { url = "http://entrepot.metropolegrenoble.fr/opendata/Imposition/imposition.json"
                , expect = Http.expectJson GotData Tax.decoder
                }

        request2 =
            Http.get
                { url = "https://pastebin.com/raw/ec5VV21w"
                , expect = Http.expectString GotMoreData
                }
    in
    ( Model coord1 coord2 Loading [] Nothing Nothing Nothing
    , Cmd.batch
        [ request1
        , request2
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData (Ok data) ->
            ( { model
                | taxes = Success data
                , cities = Tax.cities data
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
            ( model, Astronaut.parseCSV data )

        GotMoreData (Err _) ->
            ( model, Cmd.none )

        Parsed value ->
            let
                parsed =
                    Json.Decode.decodeValue Astronaut.decoder value
            in
            ( { model | csvValue = Result.toMaybe parsed }, Cmd.none )

        SelectFile ->
            ( model, File.Select.file [ "text/csv" ] FileSelected )

        FileSelected file ->
            ( { model | file = Just file }, Task.perform FileLoaded (File.toString file) )

        FileLoaded _ ->
            -- let
            --     _ =
            --         Debug.log "file" str
            -- in
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Astronaut.results Parsed


viewPoints : Result Json.Decode.Error (List Point) -> Result Json.Decode.Error (List Point) -> Html msg
viewPoints rp1 rp2 =
    case ( rp1, rp2 ) of
        ( Ok p1, Ok p2 ) ->
            Point.viewBoth p1 p2

        ( Ok p1, Err e2 ) ->
            Html.div []
                [ Point.viewOne p1
                , Point.viewError e2
                ]

        ( Err e1, Ok p2 ) ->
            Html.div []
                [ Point.viewOne p2
                , Point.viewError e1
                ]

        ( Err e1, Err e2 ) ->
            Html.div []
                [ Point.viewError e1
                , Point.viewError e2
                ]


viewTaxes : Maybe String -> WebData (List Tax) -> Html msg
viewTaxes city webTaxes =
    case webTaxes of
        Success taxes ->
            Tax.view city taxes

        Failure error ->
            Html.text (Utils.errorToString error)

        Loading ->
            Html.text "Loading..."

        NotAsked ->
            Html.text ""


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ Html.Events.onClick SelectFile ] [ Html.text "Select File" ]
        , viewPoints model.points1 model.points2
        , Html.select [ Html.Attributes.value (Maybe.withDefault "" model.selectedCity), Utils.onChange SelectCity ]
            (List.map (\city -> Html.option [ Html.Attributes.value city ] [ Html.text city ]) ("" :: model.cities))
        , viewTaxes model.selectedCity model.taxes
        , model.csvValue
            |> Maybe.map Astronaut.view
            |> Maybe.withDefault (Html.text "")
        ]
