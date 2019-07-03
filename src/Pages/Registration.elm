module Pages.Registration exposing (Msg(..))

import Http



-- MODEL


type alias Model =
    { username : String
    , password : String
    , repeatPassword : String
    , platform : Maybe Platform
    , hasAcceptedTerm : Bool
    , errors : List String
    }


type Platform
    = Aws
    | Azure
    | Heroku
    | SelfHosted


init : Model
init =
    { username = ""
    , password = ""
    , repeatPassword = ""
    , platform = Nothing
    , hasAcceptedTerm = False
    , errors = []
    }



-- UPDATE


type Msg
    = ChangePassword String
    | ChangeRepeatPassword String
    | ChangeUsername String
    | SelectPlatform Platform
    | ToogleAcceptTerms Bool
    | StartRegistration
    | FinishRegistration (Result Http.Error String)
