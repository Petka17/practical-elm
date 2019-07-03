module Pages.SavedPlansPage exposing
    ( Model
    , Msg(..)
    , init
    , render
    , update
    )

import Attrs exposing (..)
import Colors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Http
import SavedPlans exposing (..)
import Settings exposing (serverBaseUrl)
import Utils exposing (httpErrorString)



--MODEL


type Model
    = Loading
    | Error String
    | Loaded (List SavedPlan)


init : Maybe String -> ( Model, Cmd Msg )
init sessionId =
    ( Loading, requestPlans sessionId )



--UPDATE


type Msg
    = FinishPlans (Result Http.Error (List SavedPlan))
    | ShowPlan String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FinishPlans (Ok plans) ->
            ( Loaded plans, Cmd.none )

        FinishPlans (Err err) ->
            ( Error <| httpErrorString err, Cmd.none )

        ShowPlan _ ->
            ( model, Cmd.none )


requestPlans : Maybe String -> Cmd Msg
requestPlans sessionId =
    Http.request
        { method = "GET"
        , headers = [ Http.header "SessionId" <| Maybe.withDefault "" sessionId ]
        , url = serverBaseUrl ++ "plans"
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectJson FinishPlans decodeSavedPlans
        }



--VIEW


type alias PlanVersionExt =
    { name : String
    , createdAt : String
    , version : Int
    , planText : String
    }


render : Model -> Element Msg
render model =
    case model of
        Loading ->
            text "Loading"

        Error err ->
            text err

        Loaded list ->
            renderTable list


renderTable : List SavedPlan -> Element Msg
renderTable list =
    let
        annotateVersion : String -> PlanVersion -> PlanVersionExt
        annotateVersion name { createdAt, version, planText } =
            PlanVersionExt name createdAt version planText

        annotateVersions : SavedPlan -> List PlanVersionExt
        annotateVersions { versions, name } =
            versions |> List.map (annotateVersion name)

        tableAttrs : List (Attribute msg)
        tableAttrs =
            [ width <| px 800
            , paddingEach <| EachSide 10 10 50 10
            , spacingXY 10 10
            , centerX
            ]

        headerAttrs =
            [ Background.color lightGrey
            , Border.color darkCharcoal
            , Border.widthEach <| EachSide 0 0 1 0
            , Font.bold
            , centerX
            ]
    in
    table tableAttrs
        { data = List.concatMap annotateVersions list
        , columns =
            [ { header = el headerAttrs <| text "Plan Name"
              , width = fill
              , view =
                    \plan ->
                        el
                            [ Font.underline
                            , mouseOver [ Font.color lightCharcoal ]
                            , onClick <| ShowPlan plan.planText
                            ]
                        <|
                            text plan.name
              }
            , { header = el headerAttrs <| text "Creation time"
              , width = fill
              , view = .createdAt >> text
              }
            , { header = el headerAttrs <| text "Version"
              , width = fill
              , view = .version >> String.fromInt >> text
              }
            ]
        }
