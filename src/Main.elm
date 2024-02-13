port module Main exposing (main)

import Char exposing (isHexDigit)
import Dict
import Html exposing (i)
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


type alias Ballots =
    List Ballot


type alias Ballot =
    List Int


type alias VoteWeight =
    Int


type alias BallotWeight =
    List VoteWeight


type alias RemainingSeats =
    Int


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


type alias CandidatePosition =
    Int


walkHelper : Ballots -> BallotWeight -> RemainingSeats -> Elected -> Elected
walkHelper ballots ballotWeight remainingSeats elected =
    if remainingSeats == 0 then
        elected

    else
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

            firstPreferences : List ( CandidatePosition, VoteWeight )
            firstPreferences =
                List.map2 Tuple.pair ballots ballotWeight |> List.map fn

            initialVotes : Dict.Dict CandidatePosition VoteWeight
            initialVotes =
                Dict.empty
        in
        firstPreferences
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
            |> (\a -> [])
