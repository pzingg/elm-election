module Routing exposing(delta2url, location2messages)

import Dict
import Erl exposing (queryToString)
import Navigation exposing (Location)
import RouteUrl exposing (HistoryEntry(..), UrlChange)

import App exposing (Model, Msg(..), makeQuery, validState)


delta2url : Model -> Model -> Maybe UrlChange
delta2url _ model =
    let
        url = makeQuery model Erl.new
    in
        Just (UrlChange NewEntry ("/" ++ (queryToString url)))


location2messages : Location -> List Msg
location2messages location =
    let
        url = Erl.parse location.href
        query = url.query
        filteredQuery = Dict.filter (\k v -> validState k) query
    in
        if Dict.isEmpty filteredQuery
        then
            []
        else
            [ LoadFromQuery (Dict.get "contest" query) filteredQuery ]
