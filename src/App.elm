module App exposing (..)

import Dict exposing (Dict)

type Vote = Undecided | Rep | Dem

type alias State =
    { name: String
    , delegates: Int
    , vote: Vote
    , locked: Bool
    }

type alias Model =
    { districts: Dict String State
    , hover: Maybe State
    , undecided: Int
    , rep: Int
    , dem: Int
    }


{-| Maine and Nebraska award two Electoral Votes to the popular vote winner,
    and then one each to the popular vote winner in each Congressional district
    (2 in Maine, 3 in Nebraska) in their state.
-}
stateInfo : List (String, String, Int)
stateInfo =
    [
        ( "AL", "Alabama", 9 ),
        ( "AK", "Alaska", 3 ),
        ( "AZ", "Arizona", 11 ),
        ( "AR", "Arkansas", 6 ),
        ( "CA", "California", 55 ),
        ( "CO", "Colorado", 9 ),
        ( "CT", "Connecticut", 7 ),
        ( "DE", "Delaware", 3 ),
        ( "DC", "District of Columbia", 3 ),
        ( "FL", "Florida", 29 ),
        ( "GA", "Georgia", 16 ),
        ( "HI", "Hawaii", 4 ),
        ( "ID", "Idaho", 4 ),
        ( "IL", "Illinois", 20 ),
        ( "IN", "Indiana", 11 ),
        ( "IA", "Iowa", 6 ),
        ( "KS", "Kansas", 6 ),
        ( "KY", "Kentucky", 8 ),
        ( "LA", "Louisiana", 8 ),
        ( "ME", "Maine", 2 ),
        ( "ME1", "Maine-1", 1 ),
        ( "ME2", "Maine-2", 1 ),
        ( "MD", "Maryland", 10 ),
        ( "MA", "Massachusetts", 11 ),
        ( "MI", "Michigan", 16 ),
        ( "MN", "Minnesota", 10 ),
        ( "MS", "Mississippi", 6 ),
        ( "MO", "Missouri", 10 ),
        ( "MT", "Montana", 3 ),
        ( "NE", "Nebraska", 2 ),
        ( "NE1", "Nebraska-1", 1 ),
        ( "NE2", "Nebraska-2", 1 ),
        ( "NE3", "Nebraska-3", 1 ),
        ( "NV", "Nevada", 6 ),
        ( "NH", "New Hampshire", 4 ),
        ( "NJ", "New Jersey", 14 ),
        ( "NM", "New Mexico", 5 ),
        ( "NY", "New York", 29 ),
        ( "NC", "North Carolina", 15 ),
        ( "ND", "North Dakota", 3 ),
        ( "OH", "Ohio", 18 ),
        ( "OK", "Oklahoma", 7 ),
        ( "OR", "Oregon", 7 ),
        ( "PA", "Pennsylvania", 20 ),
        ( "RI", "Rhode Island", 4 ),
        ( "SC", "South Carolina", 9 ),
        ( "SD", "South Dakota", 3 ),
        ( "TN", "Tennessee", 11 ),
        ( "TX", "Texas", 38 ),
        ( "UT", "Utah", 6 ),
        ( "VT", "Vermont", 3 ),
        ( "VA", "Virginia", 13 ),
        ( "WA", "Washington", 12 ),
        ( "WV", "West Virgina", 5 ),
        ( "WI", "Wisconsin", 10 ),
        ( "WY", "Wyoming", 3 )
    ]

stateFromInfo : (String, String, Int) -> (String, State)
stateFromInfo (abbrev, name, delegates) =
    ( abbrev
    , { name = name, delegates = delegates, vote = Undecided, locked = False }
    )


initialModel : Model
initialModel =
    { districts = List.map stateFromInfo stateInfo |> Dict.fromList
    , hover = Nothing
    , undecided = 538
    , rep = 0
    , dem = 0
    }


type Msg
    = Hover String
    | MouseOut
    | ToggleVote String
    | ToggleLock String
    | Reset


init : (Model, Cmd Msg)
init =
    ( initialModel, Cmd.none )


toggleStateVote : Maybe State -> Maybe State
toggleStateVote maybeState =
    case maybeState of
        Just state ->
            case state.vote of
                Undecided ->
                    Just { state | vote = Rep }
                Rep ->
                    Just { state | vote = Dem }
                Dem ->
                    Just { state | vote = Undecided }
        Nothing ->
            Nothing


toggleStateLocked : Maybe State -> Maybe State
toggleStateLocked maybeState =
    case maybeState of
        Just state ->
            Just { state | locked = not state.locked }

        Nothing ->
            Nothing


getDelegates : Vote -> State -> Int
getDelegates vote state =
    if state.vote == vote
    then
        state.delegates
    else
        0


addDelegates : Vote -> String -> State -> Int -> Int
addDelegates vote _ state accum =
    let
        d = getDelegates vote state
    in
        accum + d


updateTotals : Model -> Model
updateTotals model =
    let
        undecided = Dict.foldl (addDelegates Undecided) 0 model.districts
        rep = Dict.foldl (addDelegates Rep) 0 model.districts
        dem = Dict.foldl (addDelegates Dem) 0 model.districts
    in
        { model | undecided = undecided, rep = rep, dem = dem }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Hover st ->
            ( { model | hover = Dict.get st model.districts }, Cmd.none )

        MouseOut ->
            ( { model | hover = Nothing }, Cmd.none )

        ToggleVote st ->
            let
                newModel = { model | districts = Dict.update st toggleStateVote model.districts }
            in
                ( updateTotals newModel
                , Cmd.none
                )

        ToggleLock st ->
            let
                maybeState = Dict.get st model.districts
            in
                ( { model | districts = Dict.update st toggleStateLocked model.districts }
                , Cmd.none
                )

        Reset ->
            ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
