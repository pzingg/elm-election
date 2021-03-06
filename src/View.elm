module View exposing (view)

import Dict exposing (Dict)
import String
import Html exposing (Html, div, p, table, tr, td, button)
import Html.Attributes exposing (disabled)
import Svg exposing (svg, path, rect, text')
import Svg.Attributes exposing
    ( id
    , height
    , width
    , viewBox
    , preserveAspectRatio
    , d
    , x, y, dy
    , fill
    , stroke
    , strokeWidth
    )
import Svg.Events exposing (..)

import App exposing
    ( Model
    , State
    , Msg(..)
    , Party(..)
    , Seat(..)
    , Vote
    , Contest(..)
    , vote2012
    , congressionalDistrict
    , hasGovernorElection
    , hasSenateElection
    )


senateInfo : String -> State -> String
senateInfo prefix state =
    let
        sen1 =
            case state.sen1Party of
                Just party ->
                    [ state.sen1Name ++ " (" ++ (toString party) ++ ")" ]
                Nothing ->
                    []
        sen2 =
            case state.sen2Party of
                Just party ->
                    [ state.sen2Name ++ " (" ++ (toString party) ++ ")" ]
                Nothing ->
                    []
        sens = sen1 ++ sen2
    in
        if List.isEmpty sens
        then
            ""
        else
            prefix ++ (String.join " and " sens) ++ "."


stateText : Model -> String
stateText model =
    case model.hover of
        Just state ->
            let
                n = state.electors
                prefix = state.name ++ ": "
                votes =
                    case model.votes.contest of
                        Pres ->
                            if n == 1
                            then
                                "1 vote, "
                            else
                                (toString n) ++ " votes, "
                        _ ->
                            ""
                suffix = "voted " ++ (toString (vote2012 state.abbr)) ++ " in 2012."
                govInfo =
                    if state.govElection
                    then
                        " Governor seat up, incumbent is "
                    else
                        " Governor: "
                governor =
                    case state.govParty of
                        Just party ->
                            govInfo ++ state.govName ++ " (" ++ (toString party) ++ ")."
                        Nothing ->
                            ""
                senators =
                    case state.sen3Party of
                        Just party ->
                            " Senate seat up, incumbent is " ++
                                state.sen3Name ++ " (" ++ (toString party) ++ "), " ++
                                senateInfo "other senator is " state
                        Nothing ->
                            senateInfo " Senators are " state
            in
                prefix ++ votes ++ suffix ++ governor ++ senators

        Nothing ->
            "Mouse over a state to see info. Click a state to change vote."


voteText : Model -> Party -> String
voteText model party =
    let
        (rep, dem) =
            case model.votes.contest of
                Pres ->
                    ( "Trump", "Clinton" )
                _ ->
                    ( "Republicans", "Democrats" )
    in
        case party of
            Undecided ->
                "Undecided: " ++ (toString model.undecided)

            TooClose ->
                "Too close to call: " ++ (toString model.tooClose)

            Rep ->
                rep ++ ": " ++ (toString model.rep)

            Dem ->
                dem ++ ": " ++ (toString model.dem)

            Ind ->
                "Independents: " ++ (toString model.ind)


viewCounts : Model -> List (Html Msg)
viewCounts model =
    let
        contest =
            case model.votes.contest of
                Sen ->
                    "Senate"
                Gov ->
                    "Governors"
                Pres ->
                    "President"
        repdem =
            [ p []
                [ Html.text contest ]
            , p []
                [ Html.text (voteText model Rep) ]
            , p []
                [ Html.text (voteText model Dem) ]
            ]
        ind =
            case model.votes.contest of
                Pres ->
                    []
                _ ->
                    [ p []
                        [ Html.text (voteText model Ind) ]
                    ]
        others =
            [ p []
                [ Html.text (voteText model TooClose) ]
            , p []
                [ Html.text (voteText model Undecided) ]
            ]
    in
        repdem ++ ind ++ others


viewButtons : Model -> List (Html Msg)
viewButtons model =
    let
        contestButtons =
            case model.votes.contest of
                Pres ->
                    [ button [ onClick Load2012 ]
                        [ Html.text "Show 2012" ]
                    , button [ onClick Compare2012 ]
                        [ Html.text "Compare 2012" ]
                    ]
                Sen ->
                    [ button [ onClick LoadSenIncumbents ]
                        [ Html.text "Show Incumbents" ]
                    ]
                Gov ->
                    [ button [ onClick LoadGovIncumbents ]
                        [ Html.text "Show Incumbents" ]
                    ]
        stdButtons =
            [ button [ disabled (cantUndo model), onClick Undo ]
                [ Html.text "Undo" ]
            , button [ disabled (cantRedo model), onClick Redo ]
                [ Html.text "Redo" ]
            , button [ onClick Reset ]
                [ Html.text "Reset" ]
            , button [ onClick <| SetContest Pres ]
                [ Html.text "President" ]
            , button [ onClick <| SetContest Sen ]
                [ Html.text "Senate" ]
            , button [ onClick <| SetContest Gov ]
                [ Html.text "Governors" ]
            ]
    in
        contestButtons ++ stdButtons


cantUndo : Model -> Bool
cantUndo model =
    List.isEmpty model.history


cantRedo : Model -> Bool
cantRedo model =
    List.isEmpty model.future


voteColor : Model -> String -> String
voteColor model st =
    let
        maybeVote = Dict.get st model.votes.winners
    in
        case maybeVote of
            Just (vote1, vote2) ->
                if (snd vote1) /= Up && (snd vote2) /= NotAVote && (fst vote1) /= (fst vote2)
                then
                    "#e0e0e0"
                else
                    case vote1 of
                        (TooClose, Up) ->
                            "#9975b9"
                        (Dem, Up) ->
                            "#0000ff"
                        (Rep, Up) ->
                            "#ff0000"
                        (Ind, Up) ->
                            "#00ff00"
                        (Dem, NotUp) ->
                            "#e0e0ff"
                        (Rep, NotUp) ->
                            "#ffe0e0"
                        (Ind, NotUp) ->
                            "#e0ffe0"
                        _ ->
                            if congressionalDistrict st
                            then
                                "#ffffaa"
                            else
                                "#cccccc"
            Nothing ->
                "#333333"


stateAttributes : Model -> String -> List (Svg.Attribute Msg)
stateAttributes model st =
    let
        canClick =
            case model.votes.contest of
                Pres ->
                    True
                Gov ->
                    hasGovernorElection st
                Sen ->
                    hasSenateElection st
        handleClick =
            if canClick
            then
                [ onClick (ToggleVote st) ]
            else
                []
    in
        [ id st
        , onMouseOver (Hover st)
        , onMouseOut MouseOut
        , fill (voteColor model st)
        , strokeWidth "2"
        , stroke "#ffffff"
        ] ++ handleClick


viewCds : Model -> List (Html Msg)
viewCds model =
    case model.votes.contest of
        Pres ->
            [ rect ( [ height "15.484", width "15.484", y "10", x "735.484" ] ++ (stateAttributes model "ME-1") )
                []
            , rect ( [ height "15.484", width "15.484", y "10", x "750.968" ] ++ (stateAttributes model "ME-2") )
                []
            , rect ( [ height "15.484", width "15.484", y "164.8906", x "332.903" ] ++ (stateAttributes model "NE-1") )
                []
            , rect ( [ height "15.484", width "15.484", y "164.8906", x "348.387" ] ++ (stateAttributes model "NE-2") )
                []
            , rect ( [ height "15.484", width "15.484", y "164.8906", x "363.871" ] ++ (stateAttributes model "NE-3") )
                []
            ]
        _ ->
            []

view : Model -> Html Msg
view model =
    table []
        [ tr []
            [ td [ id "map" ]
                [ svg [ viewBox "160 0 640 500", preserveAspectRatio "xMidYMin" ]
                    ( [ path ( [ d "M286.452,381.613L301.935,381.613L301.935,397.097L317.419,397.097L317.419,412.581L332.903,412.581L332.903,428.065L317.419,428.065L317.419,412.582L301.935,412.581L301.935,397.098L286.452,397.097L286.452,381.614L270.968,381.613L270.968,366.129L286.452,366.129Z" ] ++ (stateAttributes model "HI") )
                        []
                    , path ( [ d "M487.742,102.903L472.258,102.903L456.774,102.903L456.774,87.419L472.258,87.419L487.742,87.419L487.742,102.903L503.226,102.903L518.71,102.903L518.71,118.387L534.194,118.387L549.677,118.387L549.677,133.871L549.677,149.355L549.677,164.839L534.194,164.839L518.71,164.839L503.226,164.839L487.742,164.839L487.742,149.355L487.742,133.871L487.742,118.387Z" ] ++ (stateAttributes model "MI") )
                        []
                    , path ( [ d "M750.968,10L766.452,10L766.452,25.484L766.452,40.968L750.968,40.968L735.484,40.968L735.484,25.484L735.484,10Z" ] ++ (stateAttributes model "ME") )
                        []
                    , path ( [ d "M379.355,180.323L379.355,195.806L363.871,195.806L348.387,195.806L348.387,180.323L332.903,180.323L332.903,164.839L348.387,164.839L363.871,164.839L379.355,164.839Z" ] ++ (stateAttributes model "NE") )
                        []
                    , path ( [ d "M627.097,164.839L642.581,164.839L658.065,164.839L673.548,164.839L689.032,164.839L689.032,180.323L689.032,195.806L689.032,211.29L673.548,211.29L658.065,211.29L642.581,211.29L627.097,211.29L611.613,211.29L596.129,211.29L596.129,195.806L580.645,195.806L580.645,180.323L580.645,164.839L596.129,164.839L611.613,164.839Z" ] ++ (stateAttributes model "PA") )
                        []
                    , path ( [ d "M735.484,40.968L750.968,40.968L766.452,40.968L766.452,56.452L766.452,71.935L750.968,71.935L735.484,71.935L735.484,56.452Z" ] ++ (stateAttributes model "NH") )
                        []
                    , path ( [ d "M704.516,56.452L720,56.452L735.484,56.452L735.484,71.935L720,71.935L704.516,71.935L689.032,71.935L689.032,56.452Z" ] ++ (stateAttributes model "VT") )
                        []
                    , path ( [ d "M735.484,71.935L750.968,71.935L766.452,71.935L766.452,87.419L781.935,87.419L781.935,102.903L766.452,102.903L750.968,102.903L735.484,102.903L720,102.903L704.516,102.903L689.032,102.903L689.032,87.419L689.032,71.935L704.516,71.935L720,71.935Z" ] ++ (stateAttributes model "MA") )
                        []
                    , path ( [ d "M766.452,118.387L766.452,133.871L750.968,133.871L735.484,133.871L735.484,118.387L735.484,102.903L750.968,102.903L766.452,102.903Z" ] ++ (stateAttributes model "RI") )
                        []
                    , path ( [ d "M735.484,118.387L735.484,133.871L720,133.871L704.516,133.871L704.516,149.355L689.032,149.355L689.032,133.871L689.032,118.387L689.032,102.903L704.516,102.903L720,102.903L735.484,102.903Z" ] ++ (stateAttributes model "CT") )
                        []
                    , path ( [ d "M704.516,180.323L689.032,180.323L689.032,164.839L673.548,164.839L658.065,164.839L642.581,164.839L627.097,164.839L611.613,164.839L596.129,164.839L580.645,164.839L580.645,149.355L596.129,149.355L596.129,133.871L611.613,133.871L611.613,118.387L627.097,118.387L627.097,102.903L642.581,102.903L642.581,87.419L658.065,87.419L658.065,71.935L673.548,71.935L689.032,71.935L689.032,87.419L689.032,102.903L689.032,118.387L689.032,133.871L689.032,149.355L689.032,164.839L704.516,164.839L720,164.839L720,180.323Z" ] ++ (stateAttributes model "NY") )
                        []
                    , path ( [ d "M689.032,180.323L704.516,180.323L720,180.323L735.484,180.323L735.484,195.806L735.484,211.29L750.968,211.29L750.968,226.774L750.968,242.258L750.968,257.742L735.484,257.742L720,257.742L704.516,257.742L704.516,242.258L704.516,226.774L704.516,211.29L704.516,195.806L689.032,195.806Z" ] ++ (stateAttributes model "NJ") )
                        []
                    , path ( [ d "M689.032,195.806L704.516,195.806L704.516,211.29L704.516,226.774L704.516,242.258L689.032,242.258L689.032,226.774L689.032,211.29Z" ] ++ (stateAttributes model "DE") )
                        []
                    , path ( [ d "M689.032,226.774L689.032,242.258L704.516,242.258L704.516,257.742L704.516,273.226L689.032,273.226L689.032,257.742L673.548,257.742L673.548,242.258L658.065,242.258L658.065,226.774L642.581,226.774L627.097,226.774L611.613,226.774L611.613,211.29L627.097,211.29L642.581,211.29L658.065,211.29L673.548,211.29L689.032,211.29Z" ] ++ (stateAttributes model "MD") )
                        []
                    , path ( [ d "M611.613,226.774L611.613,242.258L596.129,242.258L580.645,242.258L580.645,226.774L580.645,211.29L580.645,195.806L596.129,195.806L596.129,211.29L611.613,211.29Z" ] ++ (stateAttributes model "WV") )
                        []
                    , path ( [ d "M549.677,180.323L565.161,180.323L565.161,164.839L580.645,164.839L580.645,180.323L580.645,195.806L580.645,211.29L580.645,226.774L580.645,242.258L565.161,242.258L549.677,242.258L534.194,242.258L518.71,242.258L518.71,226.774L518.71,211.29L518.71,195.806L518.71,180.323L518.71,164.839L534.194,164.839L534.194,180.323Z" ] ++ (stateAttributes model "OH") )
                        []
                    , path ( [ d "M425.806,87.419L441.29,87.419L456.774,87.419L456.774,102.903L456.774,118.387L472.258,118.387L472.258,133.871L472.258,149.355L456.774,149.355L441.29,149.355L425.806,149.355L425.806,133.871L425.806,118.387L425.806,102.903Z" ] ++ (stateAttributes model "WI") )
                        []
                    , path ( [ d "M379.355,118.387L379.355,102.903L394.839,102.903L410.323,102.903L410.323,87.419L425.806,87.419L425.806,102.903L425.806,118.387L425.806,133.871L425.806,149.355L410.323,149.355L394.839,149.355L379.355,149.355L379.355,133.871Z" ] ++ (stateAttributes model "MN") )
                        []
                    , path ( [ d "M410.323,149.355L425.806,149.355L425.806,164.839L425.806,180.323L410.323,180.323L394.839,180.323L379.355,180.323L379.355,164.839L379.355,149.355L394.839,149.355Z" ] ++ (stateAttributes model "IA") )
                        []
                    , path ( [ d "M363.871,149.355L348.387,149.355L332.903,149.355L332.903,133.871L348.387,133.871L363.871,133.871L379.355,133.871L379.355,149.355Z" ] ++ (stateAttributes model "ND") )
                        []
                    , path ( [ d "M363.871,164.839L348.387,164.839L332.903,164.839L332.903,149.355L348.387,149.355L363.871,149.355L379.355,149.355L379.355,164.839Z" ] ++ (stateAttributes model "SD") )
                        []
                    , path ( [ d "M518.71,180.323L518.71,195.806L518.71,211.29L518.71,226.774L518.71,242.258L503.226,242.258L487.742,242.258L472.258,242.258L472.258,226.774L487.742,226.774L487.742,211.29L487.742,195.806L487.742,180.323L487.742,164.839L503.226,164.839L518.71,164.839Z" ] ++ (stateAttributes model "IN") )
                        []
                    , path ( [ d "M487.742,195.806L487.742,211.29L487.742,226.774L472.258,226.774L472.258,242.258L456.774,242.258L441.29,242.258L441.29,226.774L441.29,211.29L425.806,211.29L425.806,195.806L410.323,195.806L410.323,180.323L425.806,180.323L425.806,164.839L425.806,149.355L441.29,149.355L456.774,149.355L472.258,149.355L472.258,164.839L472.258,180.323L487.742,180.323Z" ] ++ (stateAttributes model "IL") )
                        []
                    , path ( [ d "M379.355,180.323L394.839,180.323L410.323,180.323L410.323,195.806L425.806,195.806L425.806,211.29L441.29,211.29L441.29,226.774L441.29,242.258L425.806,242.258L410.323,242.258L394.839,242.258L394.839,226.774L394.839,211.29L394.839,195.806L379.355,195.806Z" ] ++ (stateAttributes model "MO") )
                        []
                    , path ( [ d "M379.355,195.806L394.839,195.806L394.839,211.29L394.839,226.774L379.355,226.774L363.871,226.774L348.387,226.774L348.387,211.29L348.387,195.806L363.871,195.806Z" ] ++ (stateAttributes model "KS") )
                        []
                    , path ( [ d "M301.935,180.323L286.452,180.323L270.968,180.323L270.968,164.839L286.452,164.839L301.935,164.839L317.419,164.839L317.419,180.323Z" ] ++ (stateAttributes model "WY") )
                        []
                    , path ( [ d "M286.452,164.839L270.968,164.839L255.484,164.839L255.484,149.355L270.968,149.355L286.452,149.355L301.935,149.355L301.935,164.839Z" ] ++ (stateAttributes model "MT") )
                        []
                    , path ( [ d "M301.935,180.323L317.419,180.323L332.903,180.323L348.387,180.323L348.387,195.806L348.387,211.29L348.387,226.774L332.903,226.774L317.419,226.774L301.935,226.774L301.935,211.29L301.935,195.806Z" ] ++ (stateAttributes model "CO") )
                        []
                    , path ( [ d "M270.968,195.806L270.968,180.323L286.452,180.323L301.935,180.323L301.935,195.806L301.935,211.29L301.935,226.774L286.452,226.774L270.968,226.774L270.968,211.29Z" ] ++ (stateAttributes model "UT") )
                        []
                    , path ( [ d "M270.968,195.806L270.968,211.29L270.968,226.774L255.484,226.774L240,226.774L240,211.29L240,195.806L240,180.323L255.484,180.323L270.968,180.323Z" ] ++ (stateAttributes model "NV") )
                        []
                    , path ( [ d "M255.484,180.323L240,180.323L240,164.839L240,149.355L240,133.871L255.484,133.871L255.484,149.355L255.484,164.839L270.968,164.839L270.968,180.323Z" ] ++ (stateAttributes model "ID") )
                        []
                    , path ( [ d "M193.548,87.419L209.032,87.419L209.032,71.935L224.516,71.935L240,71.935L240,87.419L240,102.903L240,118.387L224.516,118.387L209.032,118.387L209.032,133.871L193.548,133.871L193.548,118.387L178.065,118.387L178.065,102.903L178.065,87.419L178.065,71.935L193.548,71.935Z" ] ++ (stateAttributes model "WA") )
                        []
                    , path ( [ d "M178.065,118.387L193.548,118.387L193.548,133.871L209.032,133.871L209.032,118.387L224.516,118.387L240,118.387L240,133.871L240,149.355L224.516,149.355L209.032,149.355L193.548,149.355L178.065,149.355L178.065,133.871Z" ] ++ (stateAttributes model "OR") )
                        []
                    , path ( [ d "M209.032,149.355L224.516,149.355L240,149.355L240,164.839L240,180.323L240,195.806L240,211.29L240,226.774L255.484,226.774L255.484,242.258L255.484,257.742L255.484,273.226L270.968,273.226L270.968,288.71L270.968,304.194L255.484,304.194L255.484,319.677L255.484,335.161L240,335.161L224.516,335.161L209.032,335.161L193.548,335.161L193.548,319.677L193.548,304.194L178.065,304.194L178.065,288.71L178.065,273.226L178.065,257.742L178.065,242.258L178.065,226.774L178.065,211.29L178.065,195.806L178.065,180.323L178.065,164.839L178.065,149.355L193.548,149.355Z" ] ++ (stateAttributes model "CA") )
                        []
                    , path ( [ d "M255.484,226.774L270.968,226.774L286.452,226.774L301.935,226.774L301.935,242.258L301.935,257.742L301.935,273.226L301.935,288.71L286.452,288.71L270.968,288.71L270.968,273.226L255.484,273.226L255.484,257.742L255.484,242.258Z" ] ++ (stateAttributes model "AZ") )
                        []
                    , path ( [ d "M301.935,226.774L317.419,226.774L332.903,226.774L332.903,242.258L332.903,257.742L317.419,257.742L317.419,273.226L301.935,273.226L301.935,257.742L301.935,242.258Z" ] ++ (stateAttributes model "NM") )
                        []
                    , path ( [ d "M379.355,226.774L394.839,226.774L394.839,242.258L394.839,257.742L379.355,257.742L363.871,257.742L348.387,257.742L348.387,242.258L332.903,242.258L332.903,226.774L348.387,226.774L363.871,226.774Z" ] ++ (stateAttributes model "OK") )
                        []
                    , path ( [ d "M441.29,257.742L425.806,257.742L425.806,273.226L410.323,273.226L394.839,273.226L394.839,257.742L394.839,242.258L410.323,242.258L425.806,242.258L441.29,242.258L456.774,242.258L456.774,257.742Z" ] ++ (stateAttributes model "AR") )
                        []
                    , path ( [ d "M487.742,242.258L503.226,242.258L518.71,242.258L518.71,257.742L534.194,257.742L549.677,257.742L549.677,273.226L534.194,273.226L518.71,273.226L503.226,273.226L487.742,273.226L472.258,273.226L456.774,273.226L441.29,273.226L441.29,257.742L456.774,257.742L456.774,242.258L472.258,242.258Z" ] ++ (stateAttributes model "TN") )
                        []
                    , path ( [ d "M518.71,242.258L534.194,242.258L549.677,242.258L565.161,242.258L580.645,242.258L580.645,257.742L596.129,257.742L611.613,257.742L611.613,273.226L596.129,273.226L580.645,273.226L565.161,273.226L549.677,273.226L549.677,257.742L534.194,257.742L518.71,257.742Z" ] ++ (stateAttributes model "KY") )
                        []
                    , path ( [ d "M611.613,226.774L627.097,226.774L642.581,226.774L658.065,226.774L658.065,242.258L673.548,242.258L673.548,257.742L673.548,273.226L658.065,273.226L642.581,273.226L627.097,273.226L611.613,273.226L611.613,257.742L596.129,257.742L580.645,257.742L580.645,242.258L596.129,242.258L611.613,242.258Z" ] ++ (stateAttributes model "VA") )
                        []
                    , path ( [ d "M565.161,273.226L580.645,273.226L596.129,273.226L611.613,273.226L627.097,273.226L642.581,273.226L658.065,273.226L673.548,273.226L673.548,288.71L673.548,304.194L658.065,304.194L658.065,319.677L642.581,319.677L627.097,319.677L611.613,319.677L611.613,304.194L596.129,304.194L596.129,288.71L580.645,288.71L565.161,288.71Z" ] ++ (stateAttributes model "NC") )
                        []
                    , path ( [ d "M735.484,288.71L750.968,288.71L750.968,304.194L750.968,319.677L735.484,319.677L735.484,304.194L720,304.194L720,288.71Z" ] ++ (stateAttributes model "DC") )
                        []
                    , path ( [ d "M611.613,366.129L627.097,366.129L642.581,366.129L658.065,366.129L658.065,381.613L658.065,397.097L658.065,412.581L658.065,428.065L658.065,443.548L658.065,459.032L658.065,474.516L658.065,490L642.581,490L627.097,490L627.097,474.516L627.097,459.032L611.613,459.032L611.613,443.548L611.613,428.065L611.613,412.581L611.613,397.097L611.613,381.613L596.129,381.613L580.645,381.613L580.645,366.129L565.161,366.129L549.677,366.129L534.194,366.129L534.194,350.645L549.677,350.645L565.161,350.645L580.645,350.645L596.129,350.645L611.613,350.645Z" ] ++ (stateAttributes model "FL") )
                        []
                    , path ( [ d "M642.581,366.129L627.097,366.129L611.613,366.129L611.613,350.645L611.613,335.161L611.613,319.677L627.097,319.677L642.581,319.677L658.065,319.677L658.065,335.161L658.065,350.645L658.065,366.129Z" ] ++ (stateAttributes model "SC") )
                        []
                    , path ( [ d "M549.677,273.226L565.161,273.226L565.161,288.71L580.645,288.71L596.129,288.71L596.129,304.194L611.613,304.194L611.613,319.677L611.613,335.161L611.613,350.645L596.129,350.645L580.645,350.645L565.161,350.645L549.677,350.645L549.677,335.161L549.677,319.677L549.677,304.194L549.677,288.71Z" ] ++ (stateAttributes model "GA") )
                        []
                    , path ( [ d "M503.226,273.226L518.71,273.226L534.194,273.226L549.677,273.226L549.677,288.71L549.677,304.194L549.677,319.677L534.194,319.677L518.71,319.677L503.226,319.677L503.226,304.194L503.226,288.71Z" ] ++ (stateAttributes model "AL") )
                        []
                    , path ( [ d "M487.742,273.226L503.226,273.226L503.226,288.71L503.226,304.194L487.742,304.194L472.258,304.194L456.774,304.194L456.774,288.71L456.774,273.226L472.258,273.226Z" ] ++ (stateAttributes model "MS") )
                        []
                    , path ( [ d "M425.806,257.742L441.29,257.742L441.29,273.226L456.774,273.226L456.774,288.71L456.774,304.194L472.258,304.194L472.258,319.677L456.774,319.677L441.29,319.677L425.806,319.677L425.806,304.194L425.806,288.71L425.806,273.226Z" ] ++ (stateAttributes model "LA") )
                        []
                    , path ( [ d "M363.871,257.742L379.355,257.742L394.839,257.742L394.839,273.226L410.323,273.226L425.806,273.226L425.806,288.71L425.806,304.194L425.806,319.677L410.323,319.677L410.323,335.161L394.839,335.161L394.839,350.645L394.839,366.129L379.355,366.129L363.871,366.129L363.871,350.645L348.387,350.645L348.387,335.161L332.903,335.161L332.903,319.677L317.419,319.677L317.419,304.194L317.419,288.71L301.935,288.71L301.935,273.226L317.419,273.226L317.419,257.742L332.903,257.742L332.903,242.258L348.387,242.258L348.387,257.742Z" ] ++ (stateAttributes model "TX") )
                        []
                    , path ( [ d "M209.032,397.097L209.032,381.613L224.516,381.613L224.516,397.097L224.516,412.581L240,412.581L240,428.065L224.516,428.065L224.516,412.581L209.032,412.581Z" ] ++ (stateAttributes model "AK") )
                        []
                    ] ++ (viewCds model) ++
                    [ text' [ id "credit", dy "-0.5em", x "170", y "500" ]
                        [ Svg.text "Pictogram from the Wall Street Journal" ]
                    ] )
                ]
            , td [ id "legend" ]
                ( (viewCounts model) ++ (viewButtons model) ++
                [ p []
                    [ Html.text (stateText model) ]
                ] )
            ]
        ]
