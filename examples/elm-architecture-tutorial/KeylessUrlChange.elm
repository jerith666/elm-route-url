module KeylessUrlChange exposing (KeylessUrlChange(..))

import RouteUrl exposing (HistoryEntry)


type KeylessUrlChange
    = NewPath
        HistoryEntry
        { path : String
        , query : Maybe String
        , fragment : Maybe String
        }
    | NewQuery
        HistoryEntry
        { query : String
        , fragment : Maybe String
        }
    | NewFragment HistoryEntry String


