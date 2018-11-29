module Utils exposing (errorToString, onChange)

import Html exposing (Attribute)
import Html.Events
import Http
import Json.Decode


errorToString : Http.Error -> String
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


onChange : (String -> msg) -> Attribute msg
onChange handler =
    Html.Events.on "change" <| Json.Decode.map handler (Json.Decode.at [ "target", "value" ] Json.Decode.string)
