module RouteUrl exposing
    ( AppWithFlags
    , UrlChange(..), HistoryEntry(..)
    , programWithFlags, RouteUrlProgram
    , NavigationAppWithFlags, navigationAppWithFlags, runNavigationAppWithFlags
    , WrappedModel, unwrapModel, mapModel
    , WrappedMsg, unwrapMsg, wrapUserMsg, wrapLocation
    )

{-| This module provides routing for single-page apps based on changes to the
the browser's location. The routing happens in both directions
-- that is, changes to the browser's location are translated to messages
your app can respond to, and changes to your app's state are translated to
changes in the browser's location. The net effect is to make it possible for
the 'back' and 'forward' buttons in the browser to do useful things, and for
the state of your app to be partially bookmark-able.

It is, of course, possible to do something like this using
[`elm-lang/navigation`](http://package.elm-lang.org/packages/elm-lang/navigation/latest)
by itself. For a discussion of the
differences between the official module and this one, see the
[package documentation](http://package.elm-lang.org/packages/rgrempel/elm-route-url/latest).


# Configuration

You configure this module by providing the functions set out in [`App`](#App) or
[`AppWithFlags`](#AppWithFlags), depending on what kind of `init` function you
want to use.

@docs AppWithFlags


# URL Changes

You use `UrlChange` and `HistoryEntry` to indicate changes to the URL to be
displayed in the browser's location bar.

@docs UrlChange, HistoryEntry


# Initialization (the simple version)

The simplest way to use this module is to do something like this:

  - Define your [`App`](#App) or [`AppWithFlags`](#AppWithFlags) record.

  - Use [`program`](#program) or [`programWithFlags`](#programWithFlags) to
    create your `main` function, instead of their homonymous equivalents in
    [`Html`](http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html).

@docs programWithFlags, RouteUrlProgram


# More complex initialization

If your initialization needs are more complex, you may find some of the
remaining types and function to be of interest. You won't usually
need them.

@docs NavigationAppWithFlags, navigationAppWithFlags, runNavigationAppWithFlags
@docs WrappedModel, unwrapModel, mapModel
@docs WrappedMsg, unwrapMsg, wrapUserMsg, wrapLocation

-}

import Browser exposing (..)
import Browser.Navigation exposing (..)
import Dict
import Html exposing (Html)
import String exposing (startsWith)
import Update.Extra exposing (sequence)
import Url exposing (..)



-- THINGS CLIENTS PROVIDE
{- The configuration required to use this module to create a `Program`.

   The `init`, `update`, `subscriptions` and `view` fields have the same meaning
   as they do in [`Html.program`](http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html#program)
   -- that is, you should provide what you normally provide to that function.

   So, the "special" fields are the `delta2url` function and the
   `location2messages` function.

     - `delta2url` will be called when your model changes. The first parameter is
       the model's previous value, and the second is the model's new value.

       Your function should return a `Just UrlChange` if a new URL should be
       displayed in the browser's location bar (or `Nothing` if no change to the URL
       is needed). This library will check the current URL before setting a new one,
       so you need not worry about setting duplicate URLs -- that will be
       automatically avoided.

       The reason we provide both the previous and current model for your
       consideration is that sometimes you may want to do something differently
       depending on the nature of the change in the model, not just the new value.
       For instance, it might make the difference between using `NewEntry` or
       `ModifyEntry` to make the change.

       Note that this function will _not_ be called when processing messages
       returned from your `location2messages` function, since in that case the
       URL has already been set.

     - `location2messages` will be called when a change in the browser's URL is
       detected, either because the user followed a link, typed something in the
       location bar, or used the back or forward buttons.

       Note that this function will _not_ be called when your `delta2url` method
       initiates a `UrlChange` -- since in that case, the relevant change in the
       model has already occurred.

       Your function should return a list of messages that your `update` function
       can respond to. Those messages will be fed into your app, to produce the
       changes to the model that the new URL implies.

-}
{- type alias App model msg =
   { delta2url : model -> model -> Maybe UrlChange
   , location2messages : Url -> List msg
   , init : ( model, Cmd msg )
   , update : msg -> model -> ( model, Cmd msg )
   , subscriptions : model -> Sub msg
   , view : model -> Document msg
   }
-}


{-| The configuration needed to use this module to make a `Program flags`.

The `init`, `update`, `subscriptions` and `view` fields have the same meaning
as they do in
[`Html.programWithFlags`](http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html#programWithFlags)
-- that is, you should provide what you normally provide to that function.

So, the special functions are `delta2url` and `location2messages`,
which are described above, under [`App`](#App).

-}
type alias AppWithFlags model msg flags =
    { delta2url : model -> model -> Maybe UrlChange
    , location2messages : Url -> List msg
    , init : flags -> Key -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Document msg
    , onExternalUrlRequest : String -> msg
    }


{-| This works around an issue in Elm 0.18 using `programWithFlags` when
you are actually intending to ignore the flags. It's a long story.
-}
type alias AppCommon model msg =
    { delta2url : model -> model -> Maybe UrlChange
    , location2messages : Url -> List msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Document msg
    , onExternalUrlRequest : String -> msg
    }


appWithFlags2Common : AppWithFlags model msg flags -> AppCommon model msg
appWithFlags2Common app =
    { delta2url = app.delta2url
    , location2messages = app.location2messages
    , update = app.update
    , subscriptions = app.subscriptions
    , view = app.view
    , onExternalUrlRequest = app.onExternalUrlRequest
    }



-- SUPPORTING TYPES


{-| Indicates a change to be made in the URL, either creating
a new entry in the browser's history (`NewEntry`), or merely replacing the
current URL (`ModifyEntry`).

This is ultimately implemented via
[`Navigation.newUrl`](http://package.elm-lang.org/packages/elm-lang/navigation/1.0.0/Navigation#newUrl) or
[`Navigation.modifyUrl`](http://package.elm-lang.org/packages/elm-lang/navigation/1.0.0/Navigation#modifyUrl).
The reason we use this intermediate type is so that we can check whether the
provided string already corresponds to the current URL. In that case, we can
avoid creating a spurious duplicate entry in the browser's history.

The reason we take a `String` (rather than a more structured type) is that
there may be several ways you might want to build up the required URL. We
don't want to be prescriptive about that. However, the `String` you provide
must follow a couple of rules.

  - The `String` must already be uri-encoded.

  - The `String` must either start with a '/', a \`?' or a '#'.
      - If it starts with a '/', it will be interpreted as a full path, including
        optional query parameters and hash.

      - If it starts with a '?', then we'll assume that you want the current
        path to stay the same -- only the query parameters and hash will change.

      - If it starts with a '#', then we'll assume that you want the current
        path and query parameters (if any) to stay the same -- only the
        hash will change.

So, what you should _not_ provide is the scheme, host, or authentication
method -- that is, no "<http://elm-lang.org">. You should also not use relative
URLs. (Let me know if you'd like relative URLs -- we might be able to do
something sensible with them, but we don't yet in this version).

One way to construct a `UrlChange` in a modular way is to use the
`RouteUrl.Builder` module. However, a variety of approaches are possible.

-}
type UrlChange
    = NewPath
        UrlChangeMetadata
        { path : String
        , query : Maybe String
        , fragment : Maybe String
        }
    | NewQuery
        UrlChangeMetadata
        { query : String
        , fragment : Maybe String
        }
    | NewFragment UrlChangeMetadata String


type alias UrlChangeMetadata =
    { entry : HistoryEntry
    , key : Key
    }


{-| Indicates whether to create a new entry in the browser's history, or merely
modify the current entry.

One could have used a `Bool` for this instead, but I hate remembering what
`True` actually means.

-}
type HistoryEntry
    = NewEntry
    | ModifyEntry


{-| This is the router's part of the larger model.

`reportedUrl` is the last Url reported to us by the `Navigation` module.

`expectedUrlChanges` represents how many outstanding commands we've
sent to change the URL. We increment it when we send a command, and
decrement it when we get one from `Navigation` (unless it's already zero,
of course).

-}
type alias RouterModel =
    { reportedUrl : Url
    , expectedUrlChanges : Int
    }


{-| This is the model used by `RouteUrl`. In part, it is composed of the client's
model, and in part it is composed of things which `RouteUrl` wants to keep track of.
-}
type WrappedModel user
    = WrappedModel user RouterModel


{-| Given the wrapped model that `RouteUrl` uses, extract the part of the model
that your program provided.
-}
unwrapModel : WrappedModel user -> user
unwrapModel (WrappedModel user _) =
    user


{-| Given the wrapped model that `RouteUrl` uses, and a function that modifies
the part which your program provided, produce a new wrapped model.
-}
mapModel : (user -> user) -> WrappedModel user -> WrappedModel user
mapModel mapper (WrappedModel user router) =
    WrappedModel (mapper user) router


{-| This is the wrapper for RouteUrl's messages. Some messages are handled
internally by RouteUrl, and others are passed on to the application.
-}
type WrappedMsg user
    = RouterMsg Url
    | UserMsg user


{-| Given the wrapped msg type that `RouteUrl` uses, either apply a function
that works on a `Location`, or apply a function that works on the msg type
that your program uses.
-}
unwrapMsg : (Url -> a) -> (user -> a) -> WrappedMsg user -> a
unwrapMsg handleLocation handleUserMsg wrapped =
    case wrapped of
        RouterMsg location ->
            handleLocation location

        UserMsg msg ->
            handleUserMsg msg


{-| Given the kind of message your program uses, wrap it in the kind of msg
`RouteUrl` uses.
-}
wrapUserMsg : user -> WrappedMsg user
wrapUserMsg =
    UserMsg


{-| Given a location, make the kind of message that `RouteUrl` uses.

I'm not sure you'll ever need this ... perhaps for testing?

-}
wrapLocation : Url -> WrappedMsg user
wrapLocation =
    RouterMsg



-- ACTUALLY CREATING A PROGRAM


{-| A type which represents the various inputs to
[`Navigation.programWithFlags`](http://package.elm-lang.org/packages/elm-lang/navigation/2.0.0/Navigation#programWithFlags).

You can produce this via [`navigationAppWithFlags`](#navigationAppWithFlags). Then, you can supply
this to [`runNavigationAppWithFlags`](#runNavigationAppWithFlags) in order to create a `Program`.

Normally you don't need this -- you can just use [`programWithFlags`](#programWithFlags).
However, `NavigationAppWithFlags` could be useful if you want to do any further wrapping
of its functions.

-}
type alias NavigationAppWithFlags model msg flags =
    { locationToMessage : Url -> msg
    , init : flags -> Url -> Key -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Document msg
    , subscriptions : model -> Sub msg
    , onUrlRequest : UrlRequest -> msg
    }


{-| Given your configuration, this function does some wrapping and produces
the functions which
[`Navigation.programWithFlags`](http://package.elm-lang.org/packages/elm-lang/navigation/2.0.0/Navigation#programWithFlags)
requires.

Normally, you don't need this -- you can just use [`programWithFlags`](#programWithFlags).

-}
navigationAppWithFlags : AppWithFlags model msg flags -> NavigationAppWithFlags (WrappedModel model) (WrappedMsg msg) flags
navigationAppWithFlags app =
    let
        common =
            appWithFlags2Common app
    in
    { locationToMessage = RouterMsg
    , init = initWithFlags app.init common
    , update = update common
    , view = view common
    , subscriptions = subscriptions common
    , onUrlRequest = onUrlRequest common
    }


{-| Turns the output from [`navigationApp`](#navigationApp)
into a `Program` that you can assign to your `main` function.

For convenience, you will usually want to just use [`program`](#program),
which goes directly from the required
configuration to a `Program`. You would only want `runNavigationApp` for the
sake of composability -- that is, in case there is something further you want
to do with the `NavigationApp` structure before turning it into a `Program`.

-}
runNavigationAppWithFlags : NavigationAppWithFlags model msg flags -> Program flags model msg
runNavigationAppWithFlags app =
    Browser.application
        { init = app.init
        , update = app.update
        , view = app.view
        , onUrlChange = app.locationToMessage
        , onUrlRequest = app.onUrlRequest
        , subscriptions = app.subscriptions
        }


{-| A convenient alias for the `Program` type that lets you specify your
type for the `model` and `msg` ... the alias takes care of the wrapping
that `RouteUrl` supplies.

For instance, suppose your `main` function would normally be typed like this:

    main : Program Never Model Msg

Now, once you use `RouteUrl.program` to set things up, `RouteUrl` wraps your
model and msg types, so that the signature for your `main` function would
now be:

    main : Program Never (WrappedModel Model) (WrappedMsg Msg)

But that's a little ugly. So, if you like, you can use the `RouteUrlProgram`
alias like this:

    main : RouteUrlProgram Never Model Msg

It's exactly the same type, but looks a little nicer.

-}
type alias RouteUrlProgram flags model msg =
    Program flags (WrappedModel model) (WrappedMsg msg)


{-| Turns your configuration into a `Program flags` that you can assign to your
`main` function.
-}
programWithFlags : AppWithFlags model msg flags -> RouteUrlProgram flags model msg
programWithFlags =
    runNavigationAppWithFlags << navigationAppWithFlags



-- IMPLEMENTATION


{-| Call the provided view function with the user's part of the model
-}
view : AppCommon model msg -> WrappedModel model -> Document (WrappedMsg msg)
view app (WrappedModel model _) =
    app.view model
        |> docMap UserMsg


docMap : (a -> msg) -> Document a -> Document msg
docMap fn doc =
    { title = doc.title
    , body = List.map (Html.map fn) doc.body
    }


{-| Call the provided subscriptions function with the user's part of the model
-}
subscriptions : AppCommon model msg -> WrappedModel model -> Sub (WrappedMsg msg)
subscriptions app (WrappedModel model _) =
    app.subscriptions model
        |> Sub.map UserMsg


onUrlRequest : AppCommon model msg -> UrlRequest -> WrappedMsg msg
onUrlRequest app req =
    case req of
        Internal location ->
            RouterMsg location

        External location ->
            app.onExternalUrlRequest location |> UserMsg


{-| Call the provided init function with the user's part of the model
-}
initWithFlags : (flags -> Key -> ( model, Cmd msg )) -> AppCommon model msg -> flags -> Url -> Key -> ( WrappedModel model, Cmd (WrappedMsg msg) )
initWithFlags appInit app flags location key =
    let
        ( userModel, command ) =
            appInit flags key
                |> sequence app.update (app.location2messages location)

        routerModel =
            { expectedUrlChanges = 0
            , reportedUrl = location
            }
    in
    ( WrappedModel userModel routerModel
    , Cmd.map UserMsg command
    )


{-| Call the provided init function with the user's part of the model
-}
init : ( model, Cmd msg ) -> AppCommon model msg -> Url -> ( WrappedModel model, Cmd (WrappedMsg msg) )
init appInit app location =
    let
        ( userModel, command ) =
            sequence app.update (app.location2messages location) appInit

        routerModel =
            { expectedUrlChanges = 0
            , reportedUrl = location
            }
    in
    ( WrappedModel userModel routerModel
    , Cmd.map UserMsg command
    )


getMetadata : UrlChange -> UrlChangeMetadata
getMetadata urlChange =
    case urlChange of
        NewPath urlChangeMetadata _ ->
            urlChangeMetadata

        NewQuery urlChangeMetadata _ ->
            urlChangeMetadata

        NewFragment urlChangeMetadata _ ->
            urlChangeMetadata


apply : Url -> UrlChange -> Url
apply url change =
    case change of
        NewPath _ c ->
            { url
                | path = c.path
                , query = c.query
                , fragment = c.fragment
            }

        NewQuery _ c ->
            { url
                | query = Just c.query
                , fragment = c.fragment
            }

        NewFragment _ c ->
            { url | fragment = Just c }


{-| Interprets the UrlChange as a Cmd
-}
urlChange2Cmd : Url -> UrlChange -> Cmd msg
urlChange2Cmd oldUrl change =
    let
        metadata =
            getMetadata change
    in
    apply oldUrl change
        |> toString
        |> (case metadata.entry of
                NewEntry ->
                    pushUrl metadata.key

                ModifyEntry ->
                    replaceUrl metadata.key
           )


checkDistinctUrl : Url -> UrlChange -> Maybe UrlChange
checkDistinctUrl old new =
    let
        newUrl =
            apply old new
    in
    case old == newUrl of
        True ->
            Nothing

        False ->
            Just new


{-| This is the normal `update` function we're providing to `Navigation`.
-}
update : AppCommon model msg -> WrappedMsg msg -> WrappedModel model -> ( WrappedModel model, Cmd (WrappedMsg msg) )
update app msg (WrappedModel user router) =
    case msg of
        RouterMsg location ->
            let
                -- This is the same, no matter which path we follow below. Basically,
                -- we're keeping track of the last reported Url (i.e. what's in the location
                -- bar now), and all the hrefs which we expect (because we've set them
                -- ourselves). So, we remove the current href from the expectations.
                newRouterModel =
                    { reportedUrl =
                        location
                    , expectedUrlChanges =
                        if router.expectedUrlChanges > 0 then
                            router.expectedUrlChanges - 1

                        else
                            0
                    }
            in
            if router.expectedUrlChanges > 0 then
                -- This is a Url change which we were expecting, because we did
                -- it in response to a change in the app's state.  So, we don't
                -- make any *further* change to the app's state here ... we
                -- just record that we've seen the Url change we expected.
                ( WrappedModel user newRouterModel
                , Cmd.none
                )

            else
                -- This is an href which came from the outside ... i.e. clicking on a link,
                -- typing in the location bar, following a bookmark. So, we need to update
                -- the app's state to correspond to the new location.
                let
                    ( newUserModel, commands ) =
                        sequence app.update (app.location2messages location) ( user, Cmd.none )
                in
                ( WrappedModel newUserModel newRouterModel
                , Cmd.map UserMsg commands
                )

        UserMsg userMsg ->
            let
                ( newUserModel, userCommand ) =
                    -- Here we "delegate" to the `update` function provided by the user
                    app.update userMsg user

                maybeUrlChange =
                    app.delta2url user newUserModel
                        |> Maybe.andThen (checkDistinctUrl router.reportedUrl)
            in
            case maybeUrlChange of
                Just urlChange ->
                    ( WrappedModel newUserModel <|
                        { reportedUrl = apply router.reportedUrl urlChange
                        , expectedUrlChanges = router.expectedUrlChanges + 1
                        }
                    , Cmd.map UserMsg <| Cmd.batch [ urlChange2Cmd router.reportedUrl urlChange, userCommand ]
                    )

                Nothing ->
                    ( WrappedModel newUserModel router
                    , Cmd.map UserMsg userCommand
                    )
