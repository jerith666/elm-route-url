module Example2.CounterPair exposing (..)

import Example2.Counter as Counter
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import KeylessUrlChange exposing (KeylessUrlChange(..))
import RouteUrl exposing (HistoryEntry(..))
import Url exposing (Url)
import Url.Builder exposing (relative, string)



-- MODEL


type alias Model =
    { topCounter : Counter.Model
    , bottomCounter : Counter.Model
    }


{-| Rewrote to move initialization from Main.elm
-}
init : Model
init =
    { topCounter = Counter.init 0
    , bottomCounter = Counter.init 0
    }



-- UPDATE


type Action
    = Reset
    | Top Counter.Action
    | Bottom Counter.Action


update : Action -> Model -> Model
update action model =
    case action of
        Reset ->
            init

        Top act ->
            { model
                | topCounter = Counter.update act model.topCounter
            }

        Bottom act ->
            { model
                | bottomCounter = Counter.update act model.bottomCounter
            }



-- VIEW


view : Model -> Html Action
view model =
    div []
        [ Html.map Top (Counter.view model.topCounter)
        , Html.map Bottom (Counter.view model.bottomCounter)
        , button [ onClick Reset ] [ text "RESET" ]
        ]


{-| We add a separate function to get a title, which the ExampleViewer uses to
construct a table of contents. Sometimes, you might have a function of this
kind return `Html` instead, depending on where it makes sense to do some of
the construction.
-}
title : String
title =
    "Pair of Counters"



-- Routing (New API)


{-| We'll put the two counters in the query parameters, just for fun
-}
delta2builder : Model -> Model -> Maybe KeylessUrlChange
delta2builder previous current =
    Just <| NewQuery NewEntry <|
    { query = (relative [] [string "top" (Counter.delta2fragment previous.topCounter current.topCounter),
        string "bottom" (Counter.delta2fragment previous.bottomCounter current.bottomCounter)]),
        fragment = Nothing}


builder2messages : Url -> List Action
builder2messages url =
    let
        left =
            getQuery "top" builder
                |> List.concatMap Counter.fragment2messages
                |> List.map Top

        right =
            getQuery "bottom" builder
                |> List.concatMap Counter.fragment2messages
                |> List.map Bottom
    in
    List.append left right
