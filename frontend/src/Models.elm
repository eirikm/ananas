module Models exposing (..)

import RemoteData exposing (RemoteData)


type Msg
    = NoOp
    | UsernameChanged String
    | UsernameSubmitted
    | SvarChanged String
    | SvarSubmitted
    | HentRegnestykke
    | NyttRegnestykke NyttRegnestykkeResponse


type alias Regnestykke =
    { a : Int
    , op : String
    , b : Int
    }


type alias BesvartRegnestykke =
    { regnestykke : Regnestykke
    , svarFraBruker : Int
    , korrekt : Bool
    }


type alias Score =
    { current : Int
    , highscore : Int
    }


type alias Model =
    { username : String
    , usernameSubmitted : Bool
    , forrigeRegnestykke : Maybe BesvartRegnestykke
    , regnestykke : RemoteData String Regnestykke
    , score : Score
    , svar : String
    }


type alias NyttRegnestykkeResponse =
    { forrigeRegnestykke : Maybe BesvartRegnestykke
    , nyttRegnestykke : Regnestykke
    , score : Score
    }
