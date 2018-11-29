port module Astronaut exposing (Astronaut, Category(..), Gender(..), Status(..), astronaut, category, decoder, gender, parseCSV, results, status, view)

import Html exposing (Html)
import Json.Decode exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (optional, required)
import Plot


port results : (Value -> msg) -> Sub msg


port parseCSV : String -> Cmd msg


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


view : List Astronaut -> Html msg
view astronauts =
    let
        genderGroup : String -> Gender -> List Astronaut -> Plot.BarGroup
        genderGroup genstr gen astros =
            astros
                |> List.filter (\astro -> astro.gender == gen)
                |> List.length
                |> toFloat
                |> List.singleton
                |> Plot.group genstr

        genders : List Astronaut -> List Plot.BarGroup
        genders astros =
            [ genderGroup "Men" Male astros, genderGroup "Women" Female astros ]

        bars =
            Plot.groups genders
    in
    Plot.viewBars bars astronauts


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


astronaut : Decoder Astronaut
astronaut =
    Json.Decode.succeed Astronaut
        |> required "astronaut" Json.Decode.string
        |> required "cumulativehoursofspaceflighttime" (Json.Decode.nullable Json.Decode.int)
        |> required "gender" (Json.Decode.string |> Json.Decode.andThen gender)
        |> required "selectionyear" Json.Decode.int
        |> required "status" (Json.Decode.string |> Json.Decode.andThen status)
        |> optional "militaryorcivilian" (Json.Decode.string |> Json.Decode.andThen category) Unknown


decoder : Decoder (List Astronaut)
decoder =
    Json.Decode.list astronaut
