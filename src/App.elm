module App exposing (..)

import Dict exposing (Dict)
import String

import Erl exposing (Url, addQuery)

type Contest = Pres | Gov | Sen

type Party = Undecided | TooClose | Rep | Dem | Ind

{- Up seats can be changed.
-}
type Seat = Up | NotUp | NotAVote

type alias Vote = (Party, Seat)

{- Senate contest has two votes per state.
-}
type alias StateVote = (Vote, Vote)

type alias Votes =
    { contest: Contest
    , winners: Dict String StateVote
    }

type alias State =
    { abbr: String
    , name: String
    , electors: Int
    , pres2012Party: Party
    , govElection: Bool
    , govParty: Maybe Party
    , govName: String
    , sen3Party: Maybe Party
    , sen3Name: String
    , sen1Party: Maybe Party
    , sen1Name: String
    , sen2Party: Maybe Party
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
    | LoadSenIncumbents
    | LoadGovIncumbents
    | LoadFromQuery (Maybe String) (Dict String String)
    | ToggleVote String
    | SetContest Contest
    | Undo
    | Redo
    | Reset


getWithDefault : a -> comparable -> Dict comparable a -> a
getWithDefault def key =
    Maybe.withDefault def << Dict.get key


noVote : Vote
noVote = (Undecided, NotAVote)


partyFromString : Maybe String -> Maybe Party
partyFromString s =
    case s of
        Just "R" ->
            Just Rep
        Just "D" ->
            Just Dem
        Just "I" ->
            Just Ind
        Just "T" ->
            Just TooClose
        Just "-" ->
            Just Undecided
        _ ->
            Nothing


partyToString : Party -> String
partyToString party =
    case party of
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


contestFromString : String -> Contest
contestFromString s =
    case s of
        "Sen" ->
            Sen
        "Gov" ->
            Gov
        _ ->
            Pres


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
    , ( "OH", { abbr = "OH", name = "Ohio", electors = 18, pres2012Party = Dem, govElection = False, govParty = Just Rep, govName = "John Kasich", sen3Party = Just Rep, sen3Name = "Rob Portman", sen1Party = Just Dem, sen1Name = "Sherrod Brown", sen2Party = Nothing, sen2Name = "" } )
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
    , ( "WA", { abbr = "WA", name = "Washington", electors = 12, pres2012Party = Dem, govElection = True, govParty = Just Dem, govName = "Jay Inslee", sen3Party = Just Dem, sen3Name = "Patty Murray", sen1Party = Just Dem, sen1Name = "Maria Cantwell", sen2Party = Nothing, sen2Name = "" } )
    , ( "WV", { abbr = "WV", name = "West Virgina", electors = 5, pres2012Party = Rep, govElection = True, govParty = Just Dem, govName = "Earl Ray Tomblin", sen3Party = Nothing, sen3Name = "", sen1Party = Just Dem, sen1Name = "Joe Manchin", sen2Party = Just Rep, sen2Name = "Shelley Moore Capito" } )
    , ( "WI", { abbr = "WI", name = "Wisconsin", electors = 10, pres2012Party = Dem, govElection = False, govParty = Just Rep, govName = "Scott Walker", sen3Party = Just Rep, sen3Name = "Ron Johnson", sen1Party = Just Dem, sen1Name = "Tammy Baldwin", sen2Party = Nothing, sen2Name = "" } )
    , ( "WY", { abbr = "WY", name = "Wyoming", electors = 3, pres2012Party = Rep, govElection = False, govParty = Just Rep, govName = "Matt Mead", sen3Party = Nothing, sen3Name = "", sen1Party = Just Rep, sen1Name = "John Barrasso", sen2Party = Just Rep, sen2Name = "Mike Enzi" } )
    ]


pres2012 : List (String, StateVote)
pres2012 =
    List.map
        ( \( abbr, state ) -> ( abbr, ( (state.pres2012Party, Up), noVote ) ) )
        stateInfoList


stateInfo : Dict String State
stateInfo =
    stateInfoList
        |> Dict.fromList


allUndecided : Dict String StateVote
allUndecided =
    List.map ( \( st, _ ) -> ( st, ( (Undecided, Up), noVote ) ) )
        stateInfoList
        |> Dict.fromList


votes2012 : Dict String StateVote
votes2012 =
    pres2012 |> Dict.fromList


vote2012 : String -> Party
vote2012 st =
    let
        (vote1, _) = getWithDefault (noVote, noVote) st votes2012
    in
        fst vote1


initialGovForState : Bool -> State -> StateVote
initialGovForState incumbents state =
    if state.govElection then
        case state.govParty of
            Just party ->
                if incumbents
                then
                    ( (party, Up), noVote )
                else
                    ( (Undecided, Up), noVote )
            Nothing ->
                ( (Undecided, Up), noVote )
    else
        case state.govParty of
            Just party ->
                ( (party, NotUp), noVote )
            Nothing ->
                ( noVote, noVote )


initialGovernors : Bool -> Dict String StateVote
initialGovernors incumbents =
    List.map ( \( st, state ) -> ( st, initialGovForState incumbents state ) )
        stateInfoList
        |> Dict.fromList


initialSenForState : Bool -> State -> StateVote
initialSenForState incumbents state =
    let
        sen3 =
            case state.sen3Party of
                Just party ->
                    if incumbents
                    then
                        (party, Up)
                    else
                        (Undecided, Up)
                Nothing ->
                    noVote
        sen1 =
            case state.sen1Party of
                Just party ->
                    (party, NotUp)
                Nothing ->
                    noVote
        sen2 =
            case state.sen2Party of
                Just party ->
                    (party, NotUp)
                Nothing ->
                    noVote
    in
        if sen3 /= noVote
        then
            if sen1 /= noVote
            then
                (sen3, sen1)
            else
                (sen3, sen2)
        else
            (sen1, sen2)


initialSenators : Bool -> Dict String StateVote
initialSenators incumbents =
    List.map ( \( st, state ) -> ( st, initialSenForState incumbents state ) )
        stateInfoList
        |> Dict.fromList


initialVotes : Contest -> Bool -> Dict String StateVote
initialVotes contest incumbents =
    case contest of
        Pres ->
            if incumbents
            then
                votes2012
            else
                allUndecided
        Sen ->
            initialSenators incumbents
        Gov ->
            initialGovernors incumbents


savedVotes : Model -> Contest -> Dict String StateVote
savedVotes model contest =
    let
        savedVotes =
            List.filter (\votes -> votes.contest == contest) model.history
                |> List.head
    in
        case savedVotes of
            Nothing ->
                initialVotes contest False
            Just votes ->
                votes.winners


getStateVote : Model -> String -> StateVote
getStateVote model st = getWithDefault (noVote, noVote) st model.votes.winners


initialModel : Model
initialModel =
    { votes = { contest = Pres, winners = allUndecided }
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


toggleVote : Model -> Maybe StateVote -> Maybe StateVote
toggleVote model maybeVotes =
    case maybeVotes of
        Just (vote, other) ->
            case vote of
                (Undecided, Up) ->
                    Just ( (TooClose, Up), other )
                (TooClose, Up) ->
                    Just ( (Rep, Up), other )
                (Rep, Up) ->
                    Just ( (Dem, Up), other )
                (Dem, Up) ->
                    case model.votes.contest of
                        Pres ->
                            Just ( (Undecided, Up), other )
                        _ ->
                            Just ( (Ind, Up), other )
                (Ind, Up) ->
                    Just ( (Undecided, Up), other )
                _ ->
                    maybeVotes
        Nothing ->
            Nothing


congressionalDistrict : String -> Bool
congressionalDistrict st =
    String.contains "-" st


hasGovernorElection : String -> Bool
hasGovernorElection st =
    getWithDefault False st <| Dict.map (\k v -> v.govElection) stateInfo


hasSenateElection : String -> Bool
hasSenateElection st =
    getWithDefault False st <| Dict.map (\k v -> (v.sen3Party /= Nothing)) stateInfo


electorsForState : String -> Int
electorsForState st =
    let
        maybeState = Dict.get st stateInfo
    in
        case maybeState of
            Just state ->
                state.electors
            Nothing ->
                0


countForState : String -> Int
countForState st =
    let
        maybeState = Dict.get st stateInfo
    in
        case maybeState of
            Just state ->
                if st == "DC" || congressionalDistrict st
                then
                    0
                else
                    1
            Nothing ->
                0


addWinner : Int -> Vote -> (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int)
addWinner count vote (u, t, r, d, i) =
    case vote of
        (_, NotAVote) ->
            (u, t, r, d, i)

        (Rep, _) ->
            (u, t, r + count, d, i)

        (Dem, _) ->
            (u, t, r, d + count, i)

        (Ind, _) ->
            (u, t, r, d, i + count)

        (TooClose, _) ->
            (u, t + count, r, d, i)

        _ ->
            (u + count, t, r, d, i)


addVotes : (String -> Int) -> String -> StateVote -> (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int)
addVotes counter st (vote1, vote2) (u, t, r, d, i) =
    let
        count = counter st
    in
        addWinner count vote1 (u, t, r, d, i)
            |> addWinner count vote2


updateTotals : Model -> Model
updateTotals model =
    let
        adder =
            case model.votes.contest of
                Pres ->
                    addVotes electorsForState
                _ ->
                    addVotes countForState
        (undecided, tooClose, rep, dem, ind) = Dict.foldl adder (0, 0, 0, 0, 0) model.votes.winners
    in
        { model | undecided = undecided, tooClose = tooClose, rep = rep, dem = dem, ind = ind }


isActiveVote : Vote -> Bool
isActiveVote (_, seat) =
    case seat of
        Up ->
            True
        _ ->
            False


cmp2012Vote : Model -> (String, StateVote) -> (String, StateVote)
cmp2012Vote model (st, votes) =
    let
        v2012 = fst votes
        vNow  = fst <| getStateVote model st
        change =
            if isActiveVote vNow
            then
                case (fst v2012, fst vNow) of
                ( Rep , Rep ) ->
                    Undecided
                ( Dem, Dem ) ->
                    Undecided
                ( Rep, Dem ) ->
                    Dem
                ( Dem, Rep ) ->
                    Rep
                _ ->
                    Undecided
            else
                Undecided
    in
        (st, ( (change, Up), noVote ) )


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


addStateToQuery : (String, StateVote) -> Url -> Url
addStateToQuery (st, (vote1, vote2)) url =
    case vote1 of
        (party, Up) ->
            addQuery st (partyToString party) url
        _ ->
            url


makeQuery : Model -> Url -> Url
makeQuery model url =
    List.foldl addStateToQuery
        (addQuery "contest" (toString model.votes.contest) url)
        (Dict.toList model.votes.winners)


validState : String -> Bool
validState st =
    Dict.member st stateInfo


validStateForContest : Contest -> String -> Bool
validStateForContest contest st =
    case Dict.get st stateInfo of
        Just state ->
            case contest of
                Pres ->
                    True
                Gov ->
                    state.govElection
                Sen ->
                    state.sen3Party /= Nothing
        Nothing ->
            False


updateVote : (Dict String String) -> String -> StateVote -> StateVote
updateVote query st (vote1, vote2) =
    case vote1 of
        (_, Up) ->
            let
                maybeParty = partyFromString <| Dict.get st query
            in
                case maybeParty of
                    Nothing ->
                        ( vote1, vote2 )
                    Just party ->
                        ( (party, Up), vote2 )
        _ ->
            ( vote1, vote2 )


loadFromQuery : Maybe String -> Dict String String -> Model -> Votes
loadFromQuery maybeContest rawQuery model =
    let
        contest = contestFromString <| Maybe.withDefault "Pres" maybeContest
        query = Dict.filter (\st _ -> validStateForContest contest st) rawQuery
    in
        { contest = contest
        , winners = Dict.map (updateVote query) model.votes.winners
        }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Hover st ->
            ( { model | hover = Dict.get st stateInfo }, Cmd.none )

        MouseOut ->
            ( { model | hover = Nothing }, Cmd.none )

        SetContest contest ->
            if contest /= model.votes.contest
            then
                pushVotes model
                    { contest = contest
                    , winners = savedVotes model contest
                    }
            else
                ( model, Cmd.none )

        ToggleVote st ->
            let
                newWinners = Dict.update st (toggleVote model) model.votes.winners
            in
                pushVotes model
                    { contest = model.votes.contest
                    , winners = newWinners
                    }

        Load2012 ->
            pushVotes model
                { contest = Pres
                , winners = initialVotes Pres True
                }

        Compare2012 ->
            let
                newWinners = List.map (cmp2012Vote model) pres2012 |> Dict.fromList
            in
                pushVotes model
                    { contest = Pres
                    , winners = newWinners

                    }

        LoadSenIncumbents ->
            pushVotes model
                { contest = Sen
                , winners = initialVotes Sen True
                }

        LoadGovIncumbents ->
            pushVotes model
                { contest = Gov
                , winners = initialVotes Gov True
                }

        LoadFromQuery maybeContest query ->
            let
                newVotes = loadFromQuery maybeContest query model
            in
                pushVotes model newVotes

        Reset ->
            pushVotes model
                { contest = model.votes.contest
                , winners = initialVotes model.votes.contest False
                }

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
