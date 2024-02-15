port module Main exposing
    ( AddOn
    , Ballots
    , CandidatePosition
    , ElectedOrRejected(..)
    , PollData
    , Quota
    , RemainingSeats
    , VoteWeight
    , calcQuota
    , countVotes
    , electOrReject
    , main
    , recomputeWeights
    , runOneRound
    , shiftBallots
    , weightedFirstPreferences
    )

import Dict
import Platform


main : Program Poll {} msg
main =
    Platform.worker { init = init, subscriptions = \_ -> Sub.none, update = \_ -> \_ -> ( {}, Cmd.none ) }


type alias Poll =
    { candidates : List Candidate
    , seats : Int
    , ballots : Ballots
    }


type alias Candidate =
    String


type alias CandidatePosition =
    Int


type alias Ballots =
    List ( Ballot, VoteWeight )


type alias Ballot =
    List Rank


type alias Rank =
    Int


type alias VoteWeight =
    Int


type alias RemainingSeats =
    Int


type alias AddOn =
    Int


type alias Quota =
    Int


type ElectedOrRejected
    = IsElected CandidatePosition VoteWeight
    | IsRejected CandidatePosition


type alias Elected =
    List CandidatePosition


type alias Rejected =
    List CandidatePosition


type alias PollData =
    { numOfCandidates : Int
    , ballots : Ballots
    , remainingSeats : RemainingSeats
    , elected : Elected
    , rejected : Rejected
    }


init : Poll -> ( {}, Cmd msg )
init votes =
    ( {}, getVotes <| calcVotes votes )


port getVotes : String -> Cmd msg



-- Algorithm


calcVotes : Poll -> String
calcVotes poll =
    case validate poll of
        Err e ->
            e

        Ok validPoll ->
            "TODO"


validate : Poll -> Result String String
validate poll =
    let
        c =
            List.length poll.candidates
    in
    if poll.ballots |> List.all (\( b, _ ) -> List.length b == c) then
        if poll.seats <= 0 then
            Err "There must be at least one open seat"

        else
            -- TODO: Test that every ballot has aufsteigende Nummern und dass bei Gleichrang entsprechende Lücken bleiben, also 1,1,3,4,5 und nicht 1,1,2,3,4.
            Ok "good"

    else
        Err "At least one vote has not the length of the list of candidates"


defaultAddOn : AddOn
defaultAddOn =
    1


walkHelper : PollData -> Elected
walkHelper pollData =
    if pollData.remainingSeats == 0 then
        pollData.elected

    else
        walkHelper <| runOneRound pollData


runOneRound : PollData -> PollData
runOneRound { numOfCandidates, ballots, remainingSeats, elected, rejected } =
    let
        wFP : List (List VoteWeight)
        wFP =
            weightedFirstPreferences ballots

        votes : Dict.Dict CandidatePosition VoteWeight
        votes =
            countVotes numOfCandidates wFP

        quota : Quota
        quota =
            calcQuota remainingSeats defaultAddOn votes

        electedOrRejected : ElectedOrRejected
        electedOrRejected =
            electOrReject quota votes
    in
    case electedOrRejected of
        IsElected cand summarizedVoteWeight ->
            let
                newBallots : Ballots
                newBallots =
                    ballots
                        |> recomputeWeights cand quota summarizedVoteWeight wFP
                        |> shiftBallots cand
            in
            { numOfCandidates = numOfCandidates
            , ballots = newBallots
            , remainingSeats = remainingSeats - 1
            , elected = cand :: elected
            , rejected = rejected
            }

        IsRejected cand ->
            let
                newBallots : Ballots
                newBallots =
                    shiftBallots cand ballots
            in
            { numOfCandidates = numOfCandidates
            , ballots = newBallots
            , remainingSeats = remainingSeats
            , elected = elected
            , rejected = cand :: rejected
            }


weightedFirstPreferences : Ballots -> List (List VoteWeight)
weightedFirstPreferences ballots =
    ballots
        |> List.map
            (\( ballot, voteWeight ) ->
                let
                    countRankOne : Int
                    countRankOne =
                        ballot |> List.filter (\rank -> rank == 1) |> List.length
                in
                ballot
                    |> List.map
                        (\rank ->
                            if rank == 1 then
                                voteWeight // countRankOne

                            else
                                0
                        )
            )


countVotes : Int -> List (List VoteWeight) -> Dict.Dict CandidatePosition VoteWeight
countVotes numOfCandidates wFP =
    let
        initialVotes : Dict.Dict CandidatePosition VoteWeight
        initialVotes =
            List.range 1 numOfCandidates |> List.map (\i -> ( i, 0 )) |> Dict.fromList

        fn : List VoteWeight -> Dict.Dict CandidatePosition VoteWeight -> Dict.Dict CandidatePosition VoteWeight
        fn ballot votes =
            ballot
                |> List.foldl
                    (\voteWeight ( state, idx ) ->
                        if voteWeight > 0 then
                            ( state
                                |> Dict.update
                                    (idx + 1)
                                    (\val ->
                                        case val of
                                            Nothing ->
                                                Just voteWeight

                                            Just oldVoteWeight ->
                                                Just (voteWeight + oldVoteWeight)
                                    )
                            , idx + 1
                            )

                        else
                            ( state, idx + 1 )
                    )
                    ( votes, 0 )
                |> Tuple.first
    in
    wFP |> List.foldl fn initialVotes


calcQuota : RemainingSeats -> AddOn -> Dict.Dict CandidatePosition VoteWeight -> Quota
calcQuota remainingSeats addOn votes =
    let
        allVotes =
            votes |> Dict.values |> List.sum
    in
    (allVotes // (remainingSeats + addOn)) + 1


electOrReject : Quota -> Dict.Dict CandidatePosition VoteWeight -> ElectedOrRejected
electOrReject quota votes =
    let
        fn1 : CandidatePosition -> VoteWeight -> ( List CandidatePosition, VoteWeight ) -> ( List CandidatePosition, VoteWeight )
        fn1 k v ( w, current ) =
            if v >= quota then
                if v > current then
                    ( [ k ], v )

                else if v == current then
                    ( k :: w, v )

                else
                    ( w, current )

            else
                ( w, current )

        winners : ( List CandidatePosition, VoteWeight )
        winners =
            votes |> Dict.foldl fn1 ( [], 0 )
    in
    case Tuple.first winners of
        [] ->
            let
                minVoteWeight : VoteWeight
                minVoteWeight =
                    votes |> Dict.values |> List.minimum |> Maybe.withDefault 0

                loosers : List CandidatePosition
                loosers =
                    votes |> Dict.filter (\_ v -> v == minVoteWeight) |> Dict.keys
            in
            IsRejected (getSingleLooser loosers)

        first :: rest ->
            IsElected (getSingleWinner first rest) (Tuple.second winners)


getSingleWinner : CandidatePosition -> List CandidatePosition -> CandidatePosition
getSingleWinner first rest =
    if List.isEmpty rest then
        first

    else
        -- TODO: Tie break winners
        42


getSingleLooser : List CandidatePosition -> CandidatePosition
getSingleLooser loosers =
    case loosers of
        [] ->
            -- Impossible state
            0

        first :: rest ->
            if List.isEmpty rest then
                first

            else
                -- TODO: Tie break loosers
                42


recomputeWeights : CandidatePosition -> Quota -> VoteWeight -> List (List VoteWeight) -> Ballots -> Ballots
recomputeWeights cand quota summarizedVoteWeight wFP ballots =
    List.map2
        (\( ballot, _ ) voteWeights ->
            let
                total : VoteWeight
                total =
                    List.sum voteWeights

                voteWeightForCand : VoteWeight
                voteWeightForCand =
                    voteWeights |> List.drop (cand - 1) |> List.head |> Maybe.withDefault 0

                newVoteWeight : VoteWeight
                newVoteWeight =
                    total - voteWeightForCand + (voteWeightForCand * (summarizedVoteWeight - quota) // summarizedVoteWeight)
            in
            ( ballot, newVoteWeight )
        )
        ballots
        wFP


shiftBallots : CandidatePosition -> Ballots -> Ballots
shiftBallots pos ballots =
    ballots
        |> List.map
            (\( ballot, weight ) ->
                case ballot |> List.drop (pos - 1) |> List.head of
                    Nothing ->
                        -- Impossible state
                        ( ballot, weight )

                    Just rank ->
                        let
                            newBallot : Ballot
                            newBallot =
                                ballot
                                    |> List.indexedMap
                                        (\idx v ->
                                            if idx + 1 == pos then
                                                0

                                            else if v <= rank then
                                                v

                                            else
                                                v - 1
                                        )
                        in
                        ( newBallot, weight )
            )
