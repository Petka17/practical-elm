module Auth exposing (Model, Msg(..), init, login, update)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Ports exposing (..)
import Settings
import Time
import Utils exposing (httpErrorString)


type alias Model =
    { username : String
    , password : String
    , sessionId : Maybe String
    , lastError : String
    }


init : Maybe String -> Model
init sessionId =
    { username = ""
    , password = ""
    , sessionId = sessionId
    , lastError = ""
    }


type Msg
    = NoOp
    | ChangeUserName String
    | ChangePassword String
    | StartLogin
    | FinishLogin (Result Http.Error String)



-- | SendHeartBeat Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeUserName username ->
            ( { model | username = username }, Cmd.none )

        ChangePassword password ->
            ( { model | password = password }, Cmd.none )

        StartLogin ->
            ( model, login model.username model.password )

        FinishLogin (Ok sessionId) ->
            ( { model
                | sessionId = Just sessionId
                , username = ""
                , password = ""
                , lastError = ""
              }
            , Ports.saveSessionId <| Just sessionId
            )

        FinishLogin (Err err) ->
            ( { model | lastError = httpErrorString err }, Cmd.none )



-- SendHeartBeat _ ->
--     ( model, sendHeartBeat model.sessionId )


login : String -> String -> Cmd Msg
login username password =
    let
        body : Http.Body
        body =
            Http.jsonBody <|
                Encode.object
                    [ ( "username", Encode.string username )
                    , ( "password", Encode.string password )
                    ]

        responseDecoder =
            Decode.field "sessionId" Decode.string
    in
    Http.post
        { url = Settings.serverBaseUrl ++ "login"
        , body = body
        , expect = Http.expectJson FinishLogin responseDecoder
        }



-- sendHeartBeat : Maybe String -> Cmd Msg
-- sendHeartBeat sessionId =
--     Http.request
--         { method = "POST"
--         , headers = [ Http.header "SessionId" <| Maybe.withDefault "" sessionId ]
--         , url = Settings.serverBaseUrl ++ "heartbeat"
--         , body = Http.emptyBody
--         , timeout = Nothing
--         , tracker = Nothing
--         , expect = Http.expectWhatever <| always NoOp
--         }
