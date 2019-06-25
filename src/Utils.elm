module Utils exposing (httpErrorString)

import Http


httpErrorString : Http.Error -> String
httpErrorString err =
    case err of
        Http.BadBody message ->
            "Unable to handle response: " ++ message

        Http.BadStatus statusCode ->
            "Server error: " ++ String.fromInt statusCode

        Http.BadUrl url ->
            "Invalid url: " ++ url

        Http.NetworkError ->
            "Nerwork Error"

        Http.Timeout ->
            "Request Timeout"
