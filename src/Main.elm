module Main exposing (main)

import Attrs exposing (..)
import Auth
import Browser
import Browser.Events exposing (onKeyPress)
import Colors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode
import JsonPlan exposing (..)
import Pages.DisplayPage as DisplayPage
import Pages.SavedPlansPage as SavedPlansPage
import SavedPlans exposing (..)



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
    | SavedPlansPage SavedPlansPage.Model
    | InputPage
    | DisplayPage DisplayPage.Model
    | RegistrationPage


type alias Model =
    { currPage : Page
    , currPlanText : String
    , auth : Auth.Model
    , isMenuOpen : Bool
    }



-- INIT


type alias Flags =
    Maybe String


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { currPage = LoginPage
      , auth = Auth.init flags
      , isMenuOpen = False
      , currPlanText = ""
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.currPage == LoginPage then
        Sub.none

    else
        onKeyPress decodeKey


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
            OpenSavedPlans

        ( True, True, "KeyN" ) ->
            OpenInputPage

        _ ->
            NoOp



-- UPDATE


type Msg
    = OpenLoginPage
    | Auth Auth.Msg
    | OpenSavedPlans
    | SavedPlans SavedPlansPage.Msg
    | Display DisplayPage.Msg
    | ChangePlanText String
    | OpenInputPage
    | SubmitPlan
    | ToggleMenu
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

        Auth authMsg ->
            let
                ( authModel, authCmd ) =
                    Auth.update authMsg model.auth

                currPage =
                    case authMsg of
                        Auth.FinishLogin (Ok _) ->
                            InputPage

                        _ ->
                            model.currPage
            in
            ( { model
                | auth = authModel
                , currPage = currPage
              }
            , Cmd.map Auth authCmd
            )

        OpenSavedPlans ->
            let
                ( pageModel, pageCmd ) =
                    SavedPlansPage.init model.auth.sessionId
            in
            ( { model | currPage = SavedPlansPage pageModel }
            , Cmd.map SavedPlans pageCmd
            )

        SavedPlans innerMsg ->
            case innerMsg of
                SavedPlansPage.ShowPlan planText ->
                    ( { model
                        | currPage = DisplayPage <| DisplayPage.init
                        , currPlanText = planText
                      }
                    , Cmd.none
                    )

                _ ->
                    let
                        ( newModel, newCmd ) =
                            case model.currPage of
                                SavedPlansPage innerModel ->
                                    let
                                        ( newInnerModel, newInnerCmd ) =
                                            SavedPlansPage.update innerMsg innerModel
                                    in
                                    ( { model
                                        | currPage = SavedPlansPage newInnerModel
                                        , isMenuOpen = False
                                      }
                                    , Cmd.map SavedPlans newInnerCmd
                                    )

                                _ ->
                                    ( model, Cmd.none )
                    in
                    ( newModel, newCmd )

        Display innerMsg ->
            let
                newModel =
                    case model.currPage of
                        DisplayPage _ ->
                            { model
                                | currPage =
                                    DisplayPage <| DisplayPage.update innerMsg
                            }

                        _ ->
                            model
            in
            ( newModel, Cmd.none )

        OpenInputPage ->
            ( { model
                | currPage = InputPage
                , isMenuOpen = False
              }
            , Cmd.none
            )

        ChangePlanText planText ->
            ( { model | currPlanText = planText }, Cmd.none )

        SubmitPlan ->
            ( { model | currPage = DisplayPage DisplayPage.init }, Cmd.none )

        ToggleMenu ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.currPage of
                DisplayPage pageModel ->
                    DisplayPage.render model pageModel
                        |> Element.map Display

                InputPage ->
                    inputPage model

                LoginPage ->
                    loginPage model

                SavedPlansPage pageModel ->
                    SavedPlansPage.render pageModel
                        |> Element.map SavedPlans

                RegistrationPage ->
                    Debug.todo "RegistrationPage"
    in
    { title = "VisExp"
    , body =
        [ layout [ inFront <| menuPanel model.isMenuOpen ] <|
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


menuPanel : Bool -> Element Msg
menuPanel isOpen =
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
                , el [ pointer, onClick OpenSavedPlans ] <| text "Saved Plans"
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
    if isOpen then
        row [ width fill, height fill ] [ overlay, panel ]

    else
        none


loginPage : Model -> Element Msg
loginPage model =
    column [ paddingXY 0 20, spacingXY 0 10, width <| px 300, centerX ]
        [ Input.username inputField
            { onChange = Auth << Auth.ChangeUserName
            , text = model.auth.username
            , label = Input.labelAbove [] <| text "Username:"
            , placeholder = Nothing
            }
        , Input.currentPassword inputField
            { onChange = Auth << Auth.ChangePassword
            , text = model.auth.password
            , label = Input.labelAbove [] <| text "Password:"
            , placeholder = Nothing
            , show = False
            }
        , Input.button greenButton
            { onPress = Just <| Auth Auth.StartLogin
            , label = el [ centerX ] <| text "Login"
            }
        , el errorField <| text model.auth.lastError
        ]


inputPage : Model -> Element Msg
inputPage { currPlanText } =
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
            , text = currPlanText
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
