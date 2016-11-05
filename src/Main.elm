module Main exposing (..)

import Html.App exposing (program)

import App exposing (init, update, subscriptions)
import View exposing (view)


main : Program Never
main =
    program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
