module App exposing (..)

import Dict exposing (Dict)
import Task exposing (Task)
import Erl exposing (Url, addQuery)

type Contest = Pres | Gov | Sen

type Vote = Undecided | TooClose | Rep | Dem | Ind

type alias Votes = Dict String Vote

type alias Incumbent =
    { party: Maybe Vote
    , name: String
    }

type alias State =
    { abbr: String
    , name: String
    , electors: Int
    , pres2012Party: Vote
    , govElection: Bool
    , govParty: Maybe Vote
    , govName: String
    , sen3Party: Maybe Vote
    , sen3Name: String
    , sen1Party: Maybe Vote
    , sen1Name: String
    , sen2Party: Maybe Vote
    , sen2Name: String
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
    , ind: Int
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
        "I" ->
            Ind
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
        Ind ->
            "I"
        TooClose ->
            "T"
        Undecided ->
            ""


{-| Maine and Nebraska award two Electoral Votes to the popular vote winner,
    and then one each to the popular vote winner in each Congressional district
    (2 in Maine, 3 in Nebraska) in their state.
-}

stateInfoList : List (String, State)
stateInfoList =
    [ ( "AL", { abbr = "AL", name = "Alabama", electors = 9, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Robert J Bentley", sen3Party = Just Rep, sen3Name = "Richard Shelby", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "Jeff Sessions" } )
    , ( "AK", { abbr = "AK", name = "Alaska", electors = 3, pres2012Party = Rep, govElection = False, govParty = Just Ind, govName = "Bill Walker", sen3Party = Just Rep, sen3Name = "Lisa Murkowski", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "Dan Sullivan" } )
    , ( "AZ", { abbr = "AZ", name = "Arizona", electors = 11, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Doug Ducey", sen3Party = Just Rep, sen3Name = "John McCain", sen1Party = Just Rep, sen1Name = "Jeff Flake", sen2Party = Nothing, sen2Name = "" } )
    , ( "AR", { abbr = "AR", name = "Arkansas", electors = 6, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Asa Hutchinson", sen3Party = Just Rep, sen3Name = "John Boozman", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "Tom Cotton" } )
    , ( "CA", { abbr = "CA", name = "California", electors = 55, pres2012Party = Dem, govElection = False, govParty = Just Dem, govName = "Jerry Brown", sen3Party = Just Dem, sen3Name = "Barbara Boxer", sen1Party = Just Dem, sen1Name = "Dianne Feinstein", sen2Party = Nothing, sen2Name = "" } )
    , ( "CO", { abbr = "CO", name = "Colorado", electors = 9, pres2012Party = Dem, govElection = False, govParty = Just Dem, govName = "John Hickenlooper", sen3Party = Just Dem, sen3Name = "Michael Bennet", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "Cory Gardner" } )
    , ( "CT", { abbr = "CT", name = "Connecticut", electors = 7, pres2012Party = Dem, govElection = False, govParty = Just Dem, govName = "Dannel Molloy", sen3Party = Just Dem, sen3Name = "Richard Blumenthal", sen1Party = Just Dem, sen1Name = "Chris Murphy", sen2Party = Nothing, sen2Name = "" } )
    , ( "DE", { abbr = "DE", name = "Delaware", electors = 3, pres2012Party = Dem, govElection = True, govParty = Just Dem, govName = "Jack Markell", sen3Party = Nothing, sen3Name = "", sen1Party = Just Dem, sen1Name = "Tom Carper", sen2Party = Just Dem, sen2Name = "Chris Coons" } )
    , ( "DC", { abbr = "DC", name = "District of Columbia", electors = 3, pres2012Party = Dem, govElection = False, govParty = Nothing, govName = "", sen3Party = Nothing, sen3Name = "", sen1Party = Nothing, sen1Name = "", sen2Party = Nothing, sen2Name = "" } )
    , ( "FL", { abbr = "FL", name = "Florida", electors = 29, pres2012Party = Dem, govElection = False, govParty = Just Rep, govName = "Rick Scott", sen3Party = Just Rep, sen3Name = "Marco Rubio", sen1Party = Just Dem, sen1Name = "Bill Nelson", sen2Party = Nothing, sen2Name = "" } )
    , ( "GA", { abbr = "GA", name = "Georgia", electors = 16, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Nathan Deal", sen3Party = Just Rep, sen3Name = "Johnny Isakson", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "David Perdue" } )
    , ( "HI", { abbr = "HI", name = "Hawaii", electors = 4, pres2012Party = Dem, govElection = False, govParty = Just Dem, govName = "David Ige", sen3Party = Just Dem, sen3Name = "Brian Shatz", sen1Party = Just Dem, sen1Name = "Mazie Hirono", sen2Party = Nothing, sen2Name = "" } )
    , ( "ID", { abbr = "ID", name = "Idaho", electors = 4, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Butch Otter", sen3Party = Just Rep, sen3Name = "Mike Crapo", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "Jim Risch" } )
    , ( "IL", { abbr = "IL", name = "Illinois", electors = 20, pres2012Party = Dem, govElection = False, govParty = Just Dem, govName = "Bruce Rauner", sen3Party = Just Rep, sen3Name = "Mark Kirk", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "Dick Durbin" } )
    , ( "IN", { abbr = "IN", name = "Indiana", electors = 11, pres2012Party = Rep, govElection = True, govParty = Just Rep, govName = "Mike Pence", sen3Party = Just Rep, sen3Name = "Dan Coats", sen1Party = Just Dem, sen1Name = "Joe Donnelly", sen2Party = Nothing, sen2Name = "" } )
    , ( "IA", { abbr = "IA", name = "Iowa", electors = 6, pres2012Party = Dem, govElection = False, govParty = Just Rep, govName = "Terry Branstad", sen3Party = Just Rep, sen3Name = "Chuck Grassley", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "Joni Ernst" } )
    , ( "KS", { abbr = "KS", name = "Kansas", electors = 6, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Sam Brownback", sen3Party = Just Rep, sen3Name = "Jerry Moran", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "Pat Roberts" } )
    , ( "KY", { abbr = "KY", name = "Kentucky", electors = 8, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Matt Bevin", sen3Party = Just Rep, sen3Name = "Rand Paul", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "Mitch McConnell" } )
    , ( "LA", { abbr = "LA", name = "Louisiana", electors = 8, pres2012Party = Rep, govElection = False, govParty = Just Dem, govName = "John Bel Edwards", sen3Party = Just Rep, sen3Name = "David Vitter", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "Bill Cassidy" } )
    , ( "ME-1", { abbr = "ME-1", name = "Maine CD1", electors = 1, pres2012Party = Dem, govElection = False, govParty = Nothing, govName = "", sen3Party = Nothing, sen3Name = "", sen1Party = Nothing, sen1Name = "", sen2Party = Nothing, sen2Name = "" } )
    , ( "ME-2", { abbr = "ME-2", name = "Maine CD2", electors = 1, pres2012Party = Dem, govElection = False, govParty = Nothing, govName = "", sen3Party = Nothing, sen3Name = "", sen1Party = Nothing, sen1Name = "", sen2Party = Nothing, sen2Name = "" } )
    , ( "ME", { abbr = "ME", name = "Maine", electors = 2, pres2012Party = Dem, govElection = False, govParty = Just Rep, govName = "Paul LePage", sen3Party = Nothing, sen3Name = "", sen1Party = Just Ind, sen1Name = "Angus King", sen2Party = Just Rep, sen2Name = "Susan Collins" } )
    , ( "MD", { abbr = "MD", name = "Maryland", electors = 10, pres2012Party = Dem, govElection = False, govParty = Just Rep, govName = "Larry Hogan", sen3Party = Just Dem, sen3Name = "Barbara Mikulski", sen1Party = Just Dem, sen1Name = "Ben Cardin", sen2Party = Nothing, sen2Name = "" } )
    , ( "MA", { abbr = "MA", name = "Massachusetts", electors = 11, pres2012Party = Dem, govElection = False, govParty = Just Rep, govName = "Charlie Baker", sen3Party = Nothing, sen3Name = "", sen1Party = Just Dem, sen1Name = "Elizabeth Warren", sen2Party = Just Dem, sen2Name = "Ed Markey" } )
    , ( "MI", { abbr = "MI", name = "Michigan", electors = 16, pres2012Party = Dem, govElection = False, govParty = Just Rep, govName = "Rick Snyder", sen3Party = Nothing, sen3Name = "", sen1Party = Just Dem, sen1Name = "Debbie Stabenow", sen2Party = Just Dem, sen2Name = "Gary Peters" } )
    , ( "MN", { abbr = "MN", name = "Minnesota", electors = 10, pres2012Party = Dem, govElection = False, govParty = Just Dem, govName = "Mark Dayton", sen3Party = Nothing, sen3Name = "", sen1Party = Just Dem, sen1Name = "Amy Klobuchar", sen2Party = Just Dem, sen2Name = "Al Franken" } )
    , ( "MS", { abbr = "MS", name = "Mississippi", electors = 6, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Phil Bryant", sen3Party = Nothing, sen3Name = "", sen1Party = Just Rep, sen1Name = "Roger Wicker", sen2Party = Just Rep, sen2Name = "Thad Cochran" } )
    , ( "MO", { abbr = "MO", name = "Missouri", electors = 10, pres2012Party = Rep, govElection = True, govParty = Just Dem, govName = "Jay Nixon", sen3Party = Just Rep, sen3Name = "Roy Blunt", sen1Party = Just Dem, sen1Name = "Claire McCaskill", sen2Party = Nothing, sen2Name = "" } )
    , ( "MT", { abbr = "MT", name = "Montana", electors = 3, pres2012Party = Rep, govElection = True, govParty = Just Dem, govName = "Steve Bullock", sen3Party = Nothing, sen3Name = "", sen1Party = Just Dem, sen1Name = "Jon Tester", sen2Party = Just Rep, sen2Name = "Steve Daines" } )
    , ( "NE-1", { abbr = "NE-1", name = "Nebraska CD1", electors = 1, pres2012Party = Rep, govElection = False, govParty = Nothing, govName = "", sen3Party = Nothing, sen3Name = "", sen1Party = Nothing, sen1Name = "", sen2Party = Nothing, sen2Name = "" } )
    , ( "NE-2", { abbr = "NE-2", name = "Nebraska CD2", electors = 1, pres2012Party = Rep, govElection = False, govParty = Nothing, govName = "", sen3Party = Nothing, sen3Name = "", sen1Party = Nothing, sen1Name = "", sen2Party = Nothing, sen2Name = "" } )
    , ( "NE-3", { abbr = "NE-3", name = "Nebraska CD3", electors = 1, pres2012Party = Rep, govElection = False, govParty = Nothing, govName = "", sen3Party = Nothing, sen3Name = "", sen1Party = Nothing, sen1Name = "", sen2Party = Nothing, sen2Name = "" } )
    , ( "NE", { abbr = "NE", name = "Nebraska", electors = 2, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Pete Ricketts", sen3Party = Nothing, sen3Name = "", sen1Party = Just Rep, sen1Name = "Deb Fischer", sen2Party = Just Rep, sen2Name = "Ben Sasse" } )
    , ( "NV", { abbr = "NV", name = "Nevada", electors = 6, pres2012Party = Dem, govElection = False, govParty = Just Rep, govName = "Brian Sandoval", sen3Party = Just Dem, sen3Name = "Harry Reid", sen1Party = Just Rep, sen1Name = "Dean Heller", sen2Party = Nothing, sen2Name = "" } )
    , ( "NH", { abbr = "NH", name = "New Hampshire", electors = 4, pres2012Party = Dem, govElection = True, govParty = Just Dem, govName = "Maggie Hassan", sen3Party = Just Rep, sen3Name = "Kelly Ayotte", sen1Party = Nothing, sen1Name = "", sen2Party = Just Dem, sen2Name = "Jeanne Shaheen" } )
    , ( "NJ", { abbr = "NJ", name = "New Jersey", electors = 14, pres2012Party = Dem, govElection = False, govParty = Just Rep, govName = "Chris Christie", sen3Party = Nothing, sen3Name = "", sen1Party = Just Dem, sen1Name = "Bob Menendez", sen2Party = Just Dem, sen2Name = "Cory Booker" } )
    , ( "NM", { abbr = "NM", name = "New Mexico", electors = 5, pres2012Party = Dem, govElection = False, govParty = Just Rep, govName = "Susana Martinez", sen3Party = Nothing, sen3Name = "", sen1Party = Just Dem, sen1Name = "Martin Heinrich", sen2Party = Just Dem, sen2Name = "Tom Udall" } )
    , ( "NY", { abbr = "NY", name = "New York", electors = 29, pres2012Party = Dem, govElection = False, govParty = Just Dem, govName = "Andrew Cuomo", sen3Party = Just Dem, sen3Name = "Chuck Shumer", sen1Party = Just Dem, sen1Name = "Kirsten Gillibrand", sen2Party = Nothing, sen2Name = "" } )
    , ( "NC", { abbr = "NC", name = "North Carolina", electors = 15, pres2012Party = Rep, govElection = True, govParty = Just Rep, govName = "Pat McCrory", sen3Party = Just Rep, sen3Name = "Richard Burr", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "Thom Tillis" } )
    , ( "ND", { abbr = "ND", name = "North Dakota", electors = 3, pres2012Party = Rep, govElection = True, govParty = Just Rep, govName = "Jack Dalrymple", sen3Party = Just Rep, sen3Name = "John Hoeven", sen1Party = Just Dem, sen1Name = "Heidi Heitkamp", sen2Party = Nothing, sen2Name = "" } )
    , ( "OH", { abbr = "OH", name = "Ohio", electors = 18, pres2012Party = Dem, govElection = False, govParty = Just Rep, govName = "John Kasich", sen3Party = Just Dem, sen3Name = "Rob Portman", sen1Party = Just Dem, sen1Name = "Sherrod Brown", sen2Party = Nothing, sen2Name = "" } )
    , ( "OK", { abbr = "OK", name = "Oklahoma", electors = 7, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Mary Fallin", sen3Party = Just Rep, sen3Name = "James Lankford", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "Jim Inhofe" } )
    , ( "OR", { abbr = "OR", name = "Oregon", electors = 7, pres2012Party = Dem, govElection = True, govParty = Just Dem, govName = "Kate Brown", sen3Party = Just Dem, sen3Name = "Ron Wyden", sen1Party = Nothing, sen1Name = "", sen2Party = Just Dem, sen2Name = "Jeff Merkley" } )
    , ( "PA", { abbr = "PA", name = "Pennsylvania", electors = 20, pres2012Party = Dem, govElection = False, govParty = Just Dem, govName = "Tom Wolf", sen3Party = Just Rep, sen3Name = "Pat Toomey", sen1Party = Just Dem, sen1Name = "Bob Casey Jr.", sen2Party = Nothing, sen2Name = "" } )
    , ( "RI", { abbr = "RI", name = "Rhode Island", electors = 4, pres2012Party = Dem, govElection = False, govParty = Just Dem, govName = "Gina Raimondo", sen3Party = Nothing, sen3Name = "", sen1Party = Just Dem, sen1Name = "Sheldon Whitehouse", sen2Party = Just Dem, sen2Name = "Jack Reed" } )
    , ( "SC", { abbr = "SC", name = "South Carolina", electors = 9, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Nikki Haley", sen3Party = Just Rep, sen3Name = "Tim Scott", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "Lindsey Graham" } )
    , ( "SD", { abbr = "SD", name = "South Dakota", electors = 3, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Dennis Daugaard", sen3Party = Just Rep, sen3Name = "John Thune", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "Mike Rounds" } )
    , ( "TN", { abbr = "TN", name = "Tennessee", electors = 11, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Bill Haslam", sen3Party = Nothing, sen3Name = "", sen1Party = Just Rep, sen1Name = "Bob Corker", sen2Party = Just Rep, sen2Name = "Lamar Alexander" } )
    , ( "TX", { abbr = "TX", name = "Texas", electors = 38, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Greg Abbott", sen3Party = Nothing, sen3Name = "", sen1Party = Just Rep, sen1Name = "Ted Cruz", sen2Party = Just Rep, sen2Name = "John Cornyn" } )
    , ( "UT", { abbr = "UT", name = "Utah", electors = 6, pres2012Party = Rep, govElection = True, govParty = Just Rep, govName = "Gary Herbert", sen3Party = Just Rep, sen3Name = "Mike Lee", sen1Party = Just Rep, sen1Name = "Orrin Hatch", sen2Party = Nothing, sen2Name = "" } )
    , ( "VT", { abbr = "VT", name = "Vermont", electors = 3, pres2012Party = Dem, govElection = True, govParty = Just Dem, govName = "Peter Shumlin", sen3Party = Just Dem, sen3Name = "Patrick Leahy", sen1Party = Just Ind, sen1Name = "Bernie Sanders", sen2Party = Nothing, sen2Name = "" } )
    , ( "VA", { abbr = "VA", name = "Virginia", electors = 13, pres2012Party = Dem, govElection = False, govParty = Just Dem, govName = "Terry McAuliffe", sen3Party = Nothing, sen3Name = "", sen1Party = Just Dem, sen1Name = "Tim Kaine", sen2Party = Just Dem, sen2Name = "Mark Warner" } )
    , ( "WA", { abbr = "WA", name = "Washington", electors = 12, pres2012Party = Dem, govElection = True, govParty = Just Dem, govName = "Jay Inslee", sen3Party = Just Dem, sen3Name = "Patty Murray", sen1Party = Just Dem, sen1Name = "Joe Manchin", sen2Party = Nothing, sen2Name = "" } )
    , ( "WV", { abbr = "WV", name = "West Virgina", electors = 5, pres2012Party = Rep, govElection = True, govParty = Just Dem, govName = "Earl Ray Tomblin", sen3Party = Nothing, sen3Name = "", sen1Party = Nothing, sen1Name = "", sen2Party = Just Rep, sen2Name = "Shelley Moore Capito" } )
    , ( "WI", { abbr = "WI", name = "Wisconsin", electors = 10, pres2012Party = Dem, govElection = False, govParty = Just Rep, govName = "Scott Walker", sen3Party = Just Rep, sen3Name = "Ron Johnson", sen1Party = Just Dem, sen1Name = "Tammy Baldwin", sen2Party = Nothing, sen2Name = "" } )
    , ( "WY", { abbr = "WY", name = "Wyoming", electors = 3, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Matt Mead", sen3Party = Nothing, sen3Name = "", sen1Party = Just Rep, sen1Name = "John Barrasso", sen2Party = Just Rep, sen2Name = "Mike Enzi" } )
    ]


results2012 : List (String, Vote)
results2012 =
    List.map
        ( \( abbr, state ) -> ( abbr, state.pres2012Party ) )
        stateInfoList


stateInfo : Dict String State
stateInfo =
    stateInfoList
        |> Dict.fromList


initialVotes : Votes
initialVotes =
    List.map ( \( abbr, state ) -> ( abbr, Undecided ) )
        stateInfoList
            |> Dict.fromList


votes2012 : Votes
votes2012 =
    results2012
        |> Dict.fromList


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
    , ind = 0
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
                Ind ->
                    Just Undecided

        Nothing ->
            Nothing


addElectors : String -> Vote -> (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int)
addElectors st vote (u, t, r, d, i) =
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
                (u, t, r + count, d, i)

            Dem ->
                (u, t, r, d + count, i)

            Ind ->
                (u, t, r, d, i + count)

            Undecided ->
                (u + count, t, r, d, i)

            TooClose ->
                (u, t + count, r, d, i)


updateTotals : Model -> Model
updateTotals model =
    let
        (undecided, tooClose, rep, dem, ind) = Dict.foldl addElectors (0, 0, 0, 0, 0) model.votes
    in
        { model | undecided = undecided, tooClose = tooClose, rep = rep, dem = dem, ind = ind }


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
