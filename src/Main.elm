module Main exposing (..)

import RouteUrl

import App exposing (init, update, subscriptions)
import Routing exposing (delta2url, location2messages)
import View exposing (view)


main : Program Never
main =
    RouteUrl.program
        { delta2url = delta2url
        , location2messages = location2messages
        , init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
