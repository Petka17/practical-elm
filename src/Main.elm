module Main exposing (main)

import Attrs exposing (..)
import Browser
import Browser.Events exposing (onKeyPress)
import Colors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import JsonPlan exposing (..)
import Ports
import SavedPlans exposing (..)
import Time



-- CONSTANTS


serverUrl : String
serverUrl =
    "http://localhost:3000/"



-- MAIN


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type Page
    = LoginPage
    | SavedPlanList
    | InputPage
    | DisplayPage


type alias Model =
    { currPage : Page
    , username : String
    , password : String
    , lastError : String
    , sessionId : Maybe String
    , plans : List SavedPlan
    , currPlanText : String
    , selectedNode : Maybe Plan
    , isMenuOpen : Bool
    }



-- INIT


type alias Flags =
    { sessionId : Maybe String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { currPage = LoginPage
      , username = ""
      , password = ""
      , lastError = ""
      , sessionId = flags.sessionId
      , plans = []
      , currPlanText = ""
      , selectedNode = Nothing
      , isMenuOpen = False
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (10 * 1000) SendHeartBeat
        , onKeyPress decodeKey
        ]


decodeKey : Decode.Decoder Msg
decodeKey =
    Decode.map3 (\a s k -> ( a, s, k ))
        (Decode.field "altKey" Decode.bool)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.field "code" Decode.string)
        |> Decode.map keyToMsg


keyToMsg : ( Bool, Bool, String ) -> Msg
keyToMsg pressedKey =
    case pressedKey of
        ( True, True, "KeyS" ) ->
            RequestSavedPlans

        ( True, True, "KeyN" ) ->
            OpenInputPage

        _ ->
            NoOp



-- UPDATE


type Msg
    = OpenLoginPage
    | ChangeUserName String
    | ChangePassword String
    | StartLogin
    | FinishLogin (Result Http.Error String)
    | RequestSavedPlans
    | FinishPlans (Result Http.Error (List SavedPlan))
    | ShowPlan String
    | ChangePlanText String
    | OpenInputPage
    | SubmitPlan
    | GoBack
    | MouseEnterPlanNode Plan
    | MouseLeftPlanNode
    | ToggleMenu
    | SendHeartBeat Time.Posix
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenLoginPage ->
            ( { model
                | currPage = LoginPage
                , isMenuOpen = False
              }
            , Cmd.none
            )

        ChangeUserName username ->
            ( { model | username = username }, Cmd.none )

        ChangePassword password ->
            ( { model | password = password }, Cmd.none )

        StartLogin ->
            ( model, login model.username model.password )

        FinishLogin (Ok sessionId) ->
            ( { model
                | sessionId = Just sessionId
                , currPage = SavedPlanList
              }
            , Cmd.batch
                [ requestPlans <| Just sessionId
                , Ports.saveSessionId <| Just sessionId
                ]
            )

        FinishLogin (Err err) ->
            ( { model | lastError = httpErrorString err }, Cmd.none )

        RequestSavedPlans ->
            ( { model
                | currPage = SavedPlanList
                , isMenuOpen = False
              }
            , requestPlans model.sessionId
            )

        FinishPlans (Ok plans) ->
            ( { model | plans = plans }, Cmd.none )

        FinishPlans (Err err) ->
            ( model, Cmd.none )

        ShowPlan plan ->
            ( { model
                | currPage = DisplayPage
                , currPlanText = plan
              }
            , Cmd.none
            )

        OpenInputPage ->
            ( { model
                | currPage = InputPage
                , isMenuOpen = False
              }
            , Cmd.none
            )

        ChangePlanText plan ->
            ( { model | currPlanText = plan }, Cmd.none )

        SubmitPlan ->
            ( { model | currPage = DisplayPage }, Cmd.none )

        GoBack ->
            ( { model | currPage = InputPage }, Cmd.none )

        MouseEnterPlanNode plan ->
            ( { model | selectedNode = Just plan }, Cmd.none )

        MouseLeftPlanNode ->
            ( { model | selectedNode = Nothing }, Cmd.none )

        ToggleMenu ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

        SendHeartBeat _ ->
            ( model, sendHeartBeat model.sessionId )

        NoOp ->
            ( model, Cmd.none )


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
        { url = serverUrl ++ "login"
        , body = body
        , expect = Http.expectJson FinishLogin responseDecoder
        }


requestPlans : Maybe String -> Cmd Msg
requestPlans sessionId =
    Http.request
        { method = "GET"
        , headers = [ Http.header "SessionId" <| Maybe.withDefault "" sessionId ]
        , url = serverUrl ++ "plans"
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectJson FinishPlans decodeSavedPlans
        }


sendHeartBeat : Maybe String -> Cmd Msg
sendHeartBeat sessionId =
    Http.request
        { method = "POST"
        , headers = [ Http.header "SessionId" <| Maybe.withDefault "" sessionId ]
        , url = serverUrl ++ "heartbeat"
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectWhatever <| always NoOp
        }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.currPage of
                DisplayPage ->
                    displayPage model

                InputPage ->
                    inputPage model

                LoginPage ->
                    loginPage model

                SavedPlanList ->
                    savedPlanList model
    in
    { title = "VisExp"
    , body =
        [ layout [ inFront <| menuPanel model ] <|
            column [ width fill, spacingXY 0 20 ]
                [ navBar
                , content
                ]
        ]
    }


navBar : Element Msg
navBar =
    row
        [ width fill
        , paddingXY 10 10
        , Border.widthEach <| EachSide 0 0 1 0
        , Border.color blue
        ]
        [ el [ alignLeft ] <| text "VisExp"
        , Input.button
            (grayButton ++ [ padding 5, alignRight, width <| px 80 ])
            { onPress = Just ToggleMenu
            , label = el [ centerX ] <| text "Menu"
            }
        ]


menuPanel : Model -> Element Msg
menuPanel model =
    let
        panel =
            column
                [ Background.color white
                , Border.widthEach <| EachSide 0 0 0 1
                , Border.color grey
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 1
                    , blur = 10
                    , color = lightCharcoal
                    }
                , Font.bold
                , Font.color darkCharcoal
                , Font.family [ Font.sansSerif ]
                , width <| fillPortion 1
                , height fill
                , paddingXY 20 20
                , spacingXY 0 20
                ]
                [ el [ pointer, onClick OpenInputPage ] <| text "New Plan"
                , el [ pointer, onClick RequestSavedPlans ] <| text "Saved Plans"
                , el [ pointer, onClick OpenLoginPage ] <| text "Login"
                ]

        overlay =
            el
                [ width <| fillPortion 4
                , height fill
                , onClick ToggleMenu
                , Background.color <| rgba255 0 0 0 0.4
                ]
                none
    in
    if model.isMenuOpen then
        row [ width fill, height fill ] [ overlay, panel ]

    else
        none


loginPage : Model -> Element Msg
loginPage model =
    column [ paddingXY 0 20, spacingXY 0 10, width <| px 300, centerX ]
        [ Input.username inputField
            { onChange = ChangeUserName
            , text = model.username
            , label = Input.labelAbove [] <| text "Username:"
            , placeholder = Nothing
            }
        , Input.currentPassword inputField
            { onChange = ChangePassword
            , text = model.password
            , label = Input.labelAbove [] <| text "Password:"
            , placeholder = Nothing
            , show = False
            }
        , Input.button greenButton
            { onPress = Just StartLogin
            , label = el [ centerX ] <| text "Login"
            }
        , el errorField <| text model.lastError
        ]


type alias PlanVersionExt =
    { name : String, createdAt : String, version : Int, planText : String }


savedPlanList : Model -> Element Msg
savedPlanList model =
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
        { data = List.concatMap annotateVersions model.plans
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


inputPage : Model -> Element Msg
inputPage model =
    column
        [ width <| px 600
        , spacingXY 0 10
        , centerX
        ]
        [ Input.multiline
            [ height <| px 300
            , Border.width 1
            , Border.rounded 3
            , Border.color lightCharcoal
            , padding 10
            ]
            { onChange = ChangePlanText
            , text = model.currPlanText
            , placeholder = Nothing
            , spellcheck = False
            , label =
                Input.labelAbove [] <|
                    text "Paste the EXPLAIN output in JSON format:"
            }
        , Input.button
            (greenButton
                ++ [ alignRight
                   , width <| px 200
                   , height <| px 40
                   ]
            )
            { onPress = Just SubmitPlan
            , label = el [ centerX ] <| text "Go!"
            }
        ]


displayPage : Model -> Element Msg
displayPage model =
    let
        tree : List (Element Msg)
        tree =
            case Decode.decodeString decodeJsonPlan model.currPlanText of
                Ok { plan } ->
                    planNodeTree plan

                Err err ->
                    [ text <| Decode.errorToString err ]

        details : List (Element Msg)
        details =
            case model.selectedNode of
                Nothing ->
                    [ text "" ]

                Just plan ->
                    detailPanelContent plan
    in
    column [ width fill ]
        [ Input.button [ paddingXY 10 0 ]
            { onPress = Just GoBack
            , label = el [ centerX ] <| text "Back"
            }
        , row
            [ width fill
            , paddingEach <| EachSide 20 0 0 0
            ]
            [ column
                [ width (fillPortion 7)
                , height fill
                , alignTop
                ]
                tree
            , column
                [ width (fillPortion 3 |> maximum 500)
                , height fill
                , alignTop
                , padding 5
                , Border.widthEach <| EachSide 0 0 0 1
                , Border.color grey
                ]
                details
            ]
        ]


planNodeTree : Plan -> List (Element Msg)
planNodeTree plan =
    let
        nodeTypeEl nodeType =
            el [ Font.bold ] <| text nodeType

        treeNode node nodeDetails =
            [ el
                [ Border.widthEach <| EachSide 0 0 1 0
                , Border.color lightBlue
                , mouseOver [ Background.color lightYellow ]
                , padding 4
                , onMouseEnter <| MouseEnterPlanNode plan
                , onMouseLeave <| MouseLeftPlanNode
                ]
              <|
                paragraph [] (nodeTypeEl node.common.nodeType :: nodeDetails)
            , childNodeTree node.common.plans
            ]
    in
    case plan of
        PCte cteNode ->
            treeNode cteNode
                [ text " on "
                , el [ Font.italic ] <| text cteNode.cteName
                , text <| " (" ++ cteNode.alias_ ++ ")"
                ]

        PResult resultNode ->
            treeNode resultNode
                []

        PSeqScan seqScanNode ->
            treeNode seqScanNode
                [ text " on "
                , el [ Font.italic ] <| text seqScanNode.relationName
                , text <| " (" ++ seqScanNode.alias_ ++ ")"
                ]

        PSort sortNode ->
            treeNode sortNode
                [ text " on "
                , el [ Font.italic ] <| text <| String.join ", " sortNode.sortKey
                ]

        PGeneric genericNode ->
            treeNode { common = genericNode } []


childNodeTree : Plans -> Element Msg
childNodeTree (Plans plans) =
    column [ paddingEach <| EachSide 0 0 0 20 ] <|
        List.concatMap planNodeTree plans


detailPanelContent : Plan -> List (Element Msg)
detailPanelContent plan =
    let
        attr : String -> String -> Element Msg
        attr name value =
            wrappedRow [ width fill ]
                [ el
                    [ width <| px 200
                    , paddingEach <| EachSide 3 10 3 10
                    , alignTop
                    ]
                  <|
                    text name
                , paragraph [ width fill, Font.bold, scrollbarX ] [ text value ]
                ]

        header : String -> Element Msg
        header name =
            el [ paddingEach <| EachSide 10 0 5 10 ] <|
                el
                    [ Font.bold
                    , Border.widthEach <| EachSide 0 0 1 0
                    ]
                <|
                    text name

        commonAttrs : CommonFields -> List (Element Msg)
        commonAttrs common =
            [ attr "Startup Cost" <| String.fromFloat common.startupCost
            , attr "Total Cost" <| String.fromFloat common.totalCost
            , attr "Schema" <| common.schema
            ]
    in
    case plan of
        PCte node ->
            commonAttrs node.common

        PResult node ->
            commonAttrs node.common

        PSeqScan node ->
            commonAttrs node.common
                ++ [ header "Filter"
                   , attr "Filter" node.filter
                   , attr "Width" <| String.fromInt node.rowsRemovedByFilter
                   ]

        PSort node ->
            commonAttrs node.common
                ++ [ header "Sort"
                   , attr "Sort Key" <| String.join ", " node.sortKey
                   , attr "Sort Method" node.sortMethod
                   , attr "Sort Space Type" node.sortSpaceType
                   , attr "Sort Space Used" <| String.fromInt node.sortSpaceUsed
                   ]

        PGeneric node ->
            commonAttrs node
