module Example2.Counter exposing
    ( Action
    , Model
    , delta2fragment
    , delta2update
    , fragment2messages
    , init
    , location2action
    , update
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import String exposing (toInt)



-- MODEL


type alias Model =
    Int


init : Int -> Model
init count =
    count



-- UPDATE


{-| We add a Set action for the advanced example, so that we
can restore a particular bookmarked state.
-}
type Action
    = Increment
    | Decrement
    | Set Int


update : Action -> Model -> Model
update action model =
    case action of
        Increment ->
            model + 1

        Decrement ->
            model - 1

        Set value ->
            value



-- VIEW


view : Model -> Html Action
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [ countStyle ] [ text (toString model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]


countStyle : Attribute any
countStyle =
    style
        [ ( "font-size", "20px" )
        , ( "font-family", "monospace" )
        , ( "display", "inline-block" )
        , ( "width", "50px" )
        , ( "text-align", "center" )
        ]



-- Routing (New API)


{-| We'll just send back a string
-}
delta2fragment : Model -> Model -> String
delta2fragment previous current =
    toString current


{-| We'll just take a string
-}
fragment2messages : String -> List Action
fragment2messages fragment =
    case toInt fragment of
        Ok value ->
            [ Set value ]

        Err _ ->
            []
