port module Main exposing (AddOn, BallotWeights, Ballots, CandidatePosition, ElectedOrRejected(..), Quota, RemainingSeats, VoteWeight, calcQuota, countVotes, electOrReject, firstPreferences, main)

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
    List Ballot


type alias Ballot =
    List Int


type alias VoteWeight =
    Int


type alias BallotWeights =
    List VoteWeight


type alias RemainingSeats =
    Int


type alias AddOn =
    Int


type alias Quota =
    Int


type ElectedOrRejected
    = IsElected Int
    | IsRejected Int


type alias Elected =
    List String


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
    if poll.ballots |> List.all (\b -> List.length b == c) then
        if poll.seats <= 0 then
            Err "There must be at least one open seat"

        else
            -- TODO: Test that every ballot has differnt numbers (except 0)
            Ok "good"

    else
        Err "At least one vote has not the length of the list of candidates"


defaultAddOn : AddOn
defaultAddOn =
    1


walkHelper : Ballots -> BallotWeights -> RemainingSeats -> Elected -> Elected
walkHelper ballots ballotWeight remainingSeats elected =
    if remainingSeats == 0 then
        elected

    else
        let
            votes : Dict.Dict CandidatePosition VoteWeight
            votes =
                firstPreferences ballots ballotWeight
                    |> countVotes

            quota : Quota
            quota =
                calcQuota remainingSeats defaultAddOn votes
        in
        [ "winner" ]


firstPreferences : Ballots -> BallotWeights -> List ( CandidatePosition, VoteWeight )
firstPreferences ballots ballotWeight =
    let
        fn : ( Ballot, Int ) -> ( CandidatePosition, VoteWeight )
        fn ( ballot, weight ) =
            let
                votedFor : Int
                votedFor =
                    ballot
                        |> List.foldl
                            (\rank ( idx, pos ) ->
                                case pos of
                                    Just _ ->
                                        ( 0, pos )

                                    Nothing ->
                                        if rank == 1 then
                                            ( 0, Just (idx + 1) )

                                        else
                                            ( idx + 1, Nothing )
                            )
                            ( 0, Nothing )
                        |> (\( _, pos ) -> pos |> Maybe.withDefault 0)
            in
            ( votedFor, weight )
    in
    List.map2 Tuple.pair ballots ballotWeight |> List.map fn


countVotes : List ( CandidatePosition, VoteWeight ) -> Dict.Dict CandidatePosition VoteWeight
countVotes firstPrefs =
    let
        initialVotes =
            Dict.empty
    in
    firstPrefs
        |> List.foldl
            (\( votedFor, weight ) votes ->
                votes
                    |> Dict.update
                        votedFor
                        (\current ->
                            case current of
                                Nothing ->
                                    Just weight

                                Just v ->
                                    Just <| weight + v
                        )
            )
            initialVotes


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
        fn : CandidatePosition -> VoteWeight -> ( List CandidatePosition, VoteWeight ) -> ( List CandidatePosition, VoteWeight )
        fn k v ( w, current ) =
            if v >= quota then
                if v > current then
                    ( [ k ], v )

                else if v == current then
                    ( k :: w, v )

                else
                    ( w, current )

            else
                ( w, current )

        winners : List CandidatePosition
        winners =
            votes |> Dict.foldl fn ( [], 0 ) |> Tuple.first
    in
    case winners of
        [] ->
            -- TODO: Calc looser
            IsRejected 42

        first :: rest ->
            if List.isEmpty rest then
                IsElected first

            else
                -- Tie break winners
                IsRejected 42
