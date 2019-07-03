module Pages.DisplayPage exposing (Model, Msg, init, render, update)

import Attrs exposing (..)
import Colors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Json.Decode as Decode
import JsonPlan exposing (..)



-- MODEL


type alias Model =
    Maybe Plan


init : Model
init =
    Nothing



-- UPDATE


type Msg
    = MouseEnterPlanNode Plan
    | MouseLeftPlanNode


update : Msg -> Model
update msg =
    case msg of
        MouseEnterPlanNode plan ->
            Just plan

        MouseLeftPlanNode ->
            Nothing



-- VIEW


render : { a | currPlanText : String } -> Model -> Element Msg
render { currPlanText } selectedNode =
    case Decode.decodeString decodeJsonPlan currPlanText of
        Ok jsonPlan ->
            renderTreeContent
                jsonPlan
                selectedNode

        Err err ->
            text <| Decode.errorToString err


renderTreeContent : PlanJson -> Maybe Plan -> Element Msg
renderTreeContent planJson selectedNode =
    let
        details : List (Element Msg)
        details =
            case selectedNode of
                Nothing ->
                    [ text "" ]

                Just plan ->
                    detailPanelContent plan
    in
    row
        [ width fill
        , paddingEach <| EachSide 20 0 0 0
        ]
        [ column
            [ width (fillPortion 7)
            , height fill
            , alignTop
            ]
          <|
            planNodeTree planJson.plan
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
        attr : String -> String -> Element msg
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

        header : String -> Element msg
        header name =
            el [ paddingEach <| EachSide 10 0 5 10 ] <|
                el
                    [ Font.bold
                    , Border.widthEach <| EachSide 0 0 1 0
                    ]
                <|
                    text name

        commonAttrs : CommonFields -> List (Element msg)
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
