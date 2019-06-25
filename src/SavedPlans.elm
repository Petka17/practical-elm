module SavedPlans exposing (PlanVersion, SavedPlan, decodePlanVersion, decodeSavedPlan, decodeSavedPlans)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeSavedPlans : Decoder (List SavedPlan)
decodeSavedPlans =
    list decodeSavedPlan


type alias SavedPlan =
    { id : String
    , name : String
    , versions : List PlanVersion
    }


decodeSavedPlan : Decoder SavedPlan
decodeSavedPlan =
    succeed SavedPlan
        |> required "id" string
        |> required "name" string
        |> required "versions" (list decodePlanVersion)


type alias PlanVersion =
    { version : Int
    , createdAt : String
    , planText : String
    }


decodePlanVersion : Decoder PlanVersion
decodePlanVersion =
    succeed PlanVersion
        |> required "version" int
        |> required "createdAt" string
        |> required "planText" string
