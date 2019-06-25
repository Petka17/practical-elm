module JsonPlan exposing (CommonFields, Plan(..), PlanJson, Plans(..), decodeJsonPlan)

import Json.Decode as Decode exposing (Decoder, andThen, field, float, list, map, string, succeed)
import Json.Decode.Pipeline exposing (custom, optional, required)


type alias PlanJson =
    { executionTime : Float
    , plan : Plan
    , planningTime : Float
    , triggers : List String
    }


decodeJsonPlan : Decoder PlanJson
decodeJsonPlan =
    succeed PlanJson
        |> optional "Execution Time" float 0
        |> required "Plan" decodePlan
        |> optional "Planning Time" float 0
        |> optional "Triggers" (list string) []


type Plan
    = PCte CteNode
    | PResult ResultNode
    | PSeqScan SeqScanNode
    | PSort SortNode
    | PGeneric CommonFields


decodePlan : Decoder Plan
decodePlan =
    field "Node Type" string
        |> andThen decodeNode


decodeNode : String -> Decoder Plan
decodeNode nodeType =
    case nodeType of
        "CTE Scan" ->
            decodeCteNode

        "Result" ->
            decodeResultNode

        "Seq Scan" ->
            decodeSeqScanNode

        "Sort" ->
            decodeSortNode

        _ ->
            decodeGenericNode


type alias CteNode =
    { common : CommonFields
    , alias_ : String
    , cteName : String
    }


decodeCteNode : Decode.Decoder Plan
decodeCteNode =
    let
        innerDecoder =
            succeed CteNode
                |> custom decodeCommonFields
                |> required "Alias" Decode.string
                |> required "CTE Name" Decode.string
    in
    Decode.map PCte innerDecoder


type alias ResultNode =
    { common : CommonFields
    , parentRelationship : String
    }


decodeResultNode : Decoder Plan
decodeResultNode =
    let
        resultNode : Decoder ResultNode
        resultNode =
            succeed ResultNode
                |> custom decodeCommonFields
                |> required "Subplan Name" string
    in
    map PResult resultNode


type alias SeqScanNode =
    { common : CommonFields
    , alias_ : String
    , filter : String
    , relationName : String
    , rowsRemovedByFilter : Int
    }


decodeSeqScanNode : Decode.Decoder Plan
decodeSeqScanNode =
    let
        innerDecoder =
            succeed SeqScanNode
                |> custom decodeCommonFields
                |> required "Alias" Decode.string
                |> optional "Filter" Decode.string ""
                |> required "Relation Name" Decode.string
                |> optional "Rows Removed by Filter" Decode.int 0
    in
    Decode.map PSeqScan innerDecoder


type alias SortNode =
    { common : CommonFields
    , sortKey : List String
    , sortMethod : String
    , sortSpaceUsed : Int
    , sortSpaceType : String
    }


decodeSortNode : Decode.Decoder Plan
decodeSortNode =
    let
        innerDecoder =
            succeed SortNode
                |> custom decodeCommonFields
                |> required "Sort Key" (Decode.list Decode.string)
                |> required "Sort Method" Decode.string
                |> required "Sort Space Used" Decode.int
                |> required "Sort Space Type" Decode.string
    in
    Decode.map PSort innerDecoder


type Plans
    = Plans (List Plan)


decodePlans : Decoder Plans
decodePlans =
    map Plans <| list decodePlan


type alias CommonFields =
    { actualLoops : Int
    , actualTotalTime : Float
    , nodeType : String
    , plans : Plans
    , relationName : String
    , schema : String
    , startupCost : Float
    , totalCost : Float
    }


decodeCommonFields : Decoder CommonFields
decodeCommonFields =
    succeed CommonFields
        |> required "Actual Loops" Decode.int
        |> required "Actual Total Time" Decode.float
        |> required "Node Type" Decode.string
        |> optional "Plans" decodePlans (Plans [])
        |> optional "Relation Name" Decode.string ""
        |> optional "Schema" Decode.string ""
        |> required "Startup Cost" Decode.float
        |> required "Total Cost" Decode.float


decodeGenericNode : Decoder Plan
decodeGenericNode =
    map PGeneric decodeCommonFields
