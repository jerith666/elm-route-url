module MainWithOldAPI exposing (..)

import ExampleViewer exposing (Action, Model)
import RouteHash
import RouteUrl exposing (RouteUrlProgram)


main : RouteUrlProgram Never Model Action
main =
    RouteHash.program
        { prefix = RouteHash.defaultPrefix
        , delta2update = ExampleViewer.delta2update
        , location2action = ExampleViewer.location2action
        , init = ExampleViewer.init
        , update = ExampleViewer.update
        , view = ExampleViewer.view
        , subscriptions = ExampleViewer.subscriptions
        }
