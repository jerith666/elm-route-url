module MainWithJustHash exposing (..)

import ExampleViewer exposing (Action(..), Model)
import RouteUrl exposing (RouteUrlProgram)


main : RouteUrlProgram () Model Action
main =
    RouteUrl.programWithFlags
        { delta2url = ExampleViewer.delta2hash
        , location2messages = ExampleViewer.hash2messages
        , init = ExampleViewer.init
        , update = ExampleViewer.update
        , view = ExampleViewer.view
        , subscriptions = ExampleViewer.subscriptions
        , onUrlRequest = UrlRequested
        }
