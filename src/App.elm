module App exposing (..)

import Dict exposing (Dict)
import Task exposing (Task)
import Erl exposing (Url, addQuery)

type Vote = Undecided | TooClose | Rep | Dem

type alias Votes = Dict String Vote

type alias State =
    { abbr: String
    , name: String
    , electors: Int
    }

type alias Model =
    { votes: Votes
    , history: List Votes
    , future: List Votes
    , hover: Maybe State
    , undecided: Int
    , tooClose: Int
    , rep: Int
    , dem: Int
    }

type Msg
    = Hover String
    | MouseOut
    | Load2012
    | Compare2012
    | LoadFromQuery (Dict String String)
    | ToggleVote String
    | Undo
    | Redo
    | Reset


getWithDefault : a -> comparable -> Dict comparable a -> a
getWithDefault def key =
    Maybe.withDefault def << Dict.get key


voteFromString : String -> Vote
voteFromString s =
    case s of
        "R" ->
            Rep
        "D" ->
            Dem
        "T" ->
            TooClose
        _ ->
            Undecided


voteToString : Vote -> String
voteToString v =
    case v of
        Rep ->
            "R"
        Dem ->
            "D"
        TooClose ->
            "T"
        Undecided ->
            ""


{-| Maine and Nebraska award two Electoral Votes to the popular vote winner,
    and then one each to the popular vote winner in each Congressional district
    (2 in Maine, 3 in Nebraska) in their state.
-}
stateInfoList : List (String, String, Int)
stateInfoList =
    [ ( "AL", "Alabama", 9 )
    , ( "AK", "Alaska", 3 )
    , ( "AZ", "Arizona", 11 )
    , ( "AR", "Arkansas", 6 )
    , ( "CA", "California", 55 )
    , ( "CO", "Colorado", 9 )
    , ( "CT", "Connecticut", 7 )
    , ( "DE", "Delaware", 3 )
    , ( "DC", "District of Columbia", 3 )
    , ( "FL", "Florida", 29 )
    , ( "GA", "Georgia", 16 )
    , ( "HI", "Hawaii", 4 )
    , ( "ID", "Idaho", 4 )
    , ( "IL", "Illinois", 20 )
    , ( "IN", "Indiana", 11 )
    , ( "IA", "Iowa", 6 )
    , ( "KS", "Kansas", 6 )
    , ( "KY", "Kentucky", 8 )
    , ( "LA", "Louisiana", 8 )
    , ( "ME", "Maine Sen", 2 )
    , ( "ME-1", "Maine CD1", 1 )
    , ( "ME-2", "Maine CD2", 1 )
    , ( "MD", "Maryland", 10 )
    , ( "MA", "Massachusetts", 11 )
    , ( "MI", "Michigan", 16 )
    , ( "MN", "Minnesota", 10 )
    , ( "MS", "Mississippi", 6 )
    , ( "MO", "Missouri", 10 )
    , ( "MT", "Montana", 3 )
    , ( "NE", "Nebraska Sen", 2 )
    , ( "NE-1", "Nebraska CD1", 1 )
    , ( "NE-2", "Nebraska CD2", 1 )
    , ( "NE-3", "Nebraska CD3", 1 )
    , ( "NV", "Nevada", 6 )
    , ( "NH", "New Hampshire", 4 )
    , ( "NJ", "New Jersey", 14 )
    , ( "NM", "New Mexico", 5 )
    , ( "NY", "New York", 29 )
    , ( "NC", "North Carolina", 15 )
    , ( "ND", "North Dakota", 3 )
    , ( "OH", "Ohio", 18 )
    , ( "OK", "Oklahoma", 7 )
    , ( "OR", "Oregon", 7 )
    , ( "PA", "Pennsylvania", 20 )
    , ( "RI", "Rhode Island", 4 )
    , ( "SC", "South Carolina", 9 )
    , ( "SD", "South Dakota", 3 )
    , ( "TN", "Tennessee", 11 )
    , ( "TX", "Texas", 38 )
    , ( "UT", "Utah", 6 )
    , ( "VT", "Vermont", 3 )
    , ( "VA", "Virginia", 13 )
    , ( "WA", "Washington", 12 )
    , ( "WV", "West Virgina", 5 )
    , ( "WI", "Wisconsin", 10 )
    , ( "WY", "Wyoming", 3 )
    ]


stateInfo : Dict String State
stateInfo =
    List.map (\(st, name, electors) -> ( st, { abbr = st, name = name, electors = electors } ))
        stateInfoList
        |> Dict.fromList


results2012 : List (String, Vote)
results2012 =
    [ ( "AL", Rep )
    , ( "AK", Rep )
    , ( "AZ", Rep )
    , ( "AR", Rep )
    , ( "CA", Dem )
    , ( "CO", Dem )
    , ( "CT", Dem )
    , ( "DE", Dem )
    , ( "DC", Dem )
    , ( "FL", Dem )
    , ( "GA", Rep )
    , ( "HI", Dem )
    , ( "ID", Rep )
    , ( "IL", Dem )
    , ( "IN", Rep )
    , ( "IA", Dem )
    , ( "KS", Rep )
    , ( "KY", Rep )
    , ( "LA", Rep )
    , ( "ME", Dem )
    , ( "ME-1", Dem )
    , ( "ME-2", Dem )
    , ( "MD", Dem )
    , ( "MA", Dem )
    , ( "MI", Dem )
    , ( "MN", Dem )
    , ( "MS", Rep )
    , ( "MO", Rep )
    , ( "MT", Rep )
    , ( "NE", Rep )
    , ( "NE-1", Rep )
    , ( "NE-2", Rep )
    , ( "NE-3", Rep )
    , ( "NV", Dem )
    , ( "NH", Dem )
    , ( "NJ", Dem )
    , ( "NM", Dem )
    , ( "NY", Dem )
    , ( "NC", Rep )
    , ( "ND", Rep )
    , ( "OH", Dem )
    , ( "OK", Rep )
    , ( "OR", Dem )
    , ( "PA", Dem )
    , ( "RI", Dem )
    , ( "SC", Rep )
    , ( "SD", Rep )
    , ( "TN", Rep )
    , ( "TX", Rep )
    , ( "UT", Rep )
    , ( "VT", Dem )
    , ( "VA", Dem )
    , ( "WA", Dem )
    , ( "WV", Rep )
    , ( "WI", Dem )
    , ( "WY", Rep )
    ]


initialVotes : Votes
initialVotes = List.map ( \(st, vote) -> (st, Undecided) ) results2012 |> Dict.fromList


votes2012 : Votes
votes2012 =
    Dict.fromList results2012


vote2012 : String -> Vote
vote2012 st =
    getWithDefault Undecided st votes2012


voteNow : Model -> String -> Vote
voteNow model st =
    getWithDefault Undecided st model.votes


initialModel : Model
initialModel =
    { votes = initialVotes
    , history = []
    , future = []
    , hover = Nothing
    , undecided = 538
    , tooClose = 0
    , rep = 0
    , dem = 0
    }


init : (Model, Cmd Msg)
init =
    ( initialModel, Cmd.none )


toggleVote : Maybe Vote -> Maybe Vote
toggleVote maybeVote =
    case maybeVote of
        Just vote ->
            case vote of
                Undecided ->
                    Just TooClose
                TooClose ->
                    Just Rep
                Rep ->
                    Just Dem
                Dem ->
                    Just Undecided

        Nothing ->
            Nothing


addElectors : String -> Vote -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addElectors st vote (u, t, r, d) =
    let
        maybeState = Dict.get st stateInfo
        count = case maybeState of
            Just state ->
                state.electors

            Nothing ->
                0
    in
        case vote of
            Rep ->
                (u, t, r + count, d)

            Dem ->
                (u, t, r, d + count)

            Undecided ->
                (u + count, t, r, d)

            TooClose ->
                (u, t + count, r, d)


updateTotals : Model -> Model
updateTotals model =
    let
        (undecided, tooClose, rep, dem) = Dict.foldl addElectors (0, 0, 0, 0) model.votes
    in
        { model | undecided = undecided, tooClose = tooClose, rep = rep, dem = dem }


cmp2012Vote : Model -> (String, Vote) -> (String, Vote)
cmp2012Vote model (st, vote2012) =
    let
        change = case (vote2012, (voteNow model st)) of
            ( Rep, Rep ) ->
                Undecided

            ( Dem, Dem ) ->
                Undecided

            ( Rep, Dem ) ->
                Dem

            ( Dem, Rep ) ->
                Rep

            _ ->
                Undecided
    in
        (st, change)


pushVotes : Model -> Votes -> (Model, Cmd Msg)
pushVotes model newVotes =
    let
        newHistory = model.votes :: model.history
        newModel = updateTotals
            { model
            | votes = newVotes
            , history = newHistory
            , future = []
            }
    in
        ( newModel, Cmd.none )


validState : String -> Bool
validState st =
    Dict.member st votes2012


addStateToQuery : (String, Vote) -> Url -> Url
addStateToQuery (st, v) url =
    addQuery st (voteToString v) url


makeQuery : Model -> Url -> Url
makeQuery model url =
    List.foldl addStateToQuery url (Dict.toList model.votes)


loadFromQuery : Dict String String -> Model -> Votes
loadFromQuery query model =
    let
        queryVotes = Dict.map (\_ v -> voteFromString v) query
    in
        Dict.union queryVotes model.votes


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Hover st ->
            ( { model | hover = Dict.get st stateInfo }, Cmd.none )

        MouseOut ->
            ( { model | hover = Nothing }, Cmd.none )

        ToggleVote st ->
            let
                newVotes = Dict.update st toggleVote model.votes
            in
                pushVotes model newVotes

        Load2012 ->
            let
                newVotes = votes2012
            in
                pushVotes model newVotes

        Compare2012 ->
            let
                newVotes = List.map (cmp2012Vote model) results2012 |> Dict.fromList
            in
                pushVotes model newVotes

        LoadFromQuery query ->
            let
                newVotes = loadFromQuery query model
            in
                pushVotes model newVotes

        Reset ->
            let
                newVotes = initialVotes
            in
                pushVotes model newVotes

        Undo ->
            let
                newModel =
                    case model.history of
                        [] ->
                            model
                        votes :: xs ->
                            updateTotals
                                { model
                                | future = model.votes :: model.future
                                , votes = votes
                                , history = xs
                                }
            in
                ( newModel, Cmd.none )

        Redo ->
            let
                newModel =
                    case model.future of
                        [] ->
                            model
                        votes :: xs ->
                            updateTotals
                                { model
                                | history = model.votes :: model.history
                                , votes = votes
                                , future = xs
                                }
            in
                ( newModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
