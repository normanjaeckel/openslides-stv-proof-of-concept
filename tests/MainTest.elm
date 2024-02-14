module MainTest exposing (suite)

import Dict
import Expect
import Main exposing (AddOn, BallotWeights, Ballots, CandidatePosition, ElectedOrRejected(..), Quota, RemainingSeats, VoteWeight, calcQuota, countVotes, electOrReject, firstPreferences)
import Test exposing (..)


suite : Test
suite =
    describe "The"
        [ describe "firstPreferences function"
            [ test "works fine with some ballots" <|
                \_ ->
                    let
                        ballots : Ballots
                        ballots =
                            [ [ 1, 2, 3, 4, 5 ]
                            , [ 2, 1, 3, 5, 4 ]
                            , [ 1, 3, 2, 4, 5 ]
                            ]

                        ballotWeight : BallotWeights
                        ballotWeight =
                            [ 10, 20, 30 ]

                        expected : List ( CandidatePosition, VoteWeight )
                        expected =
                            [ ( 1, 10 ), ( 2, 20 ), ( 1, 30 ) ]
                    in
                    firstPreferences ballots ballotWeight
                        |> Expect.equal expected
            , test "works fine with many ballots" <|
                \_ ->
                    let
                        ballots : Ballots
                        ballots =
                            [ [ 1, 5, 2, 3, 4 ]
                            , [ 2, 1, 3, 5, 4 ]
                            , [ 4, 2, 3, 5, 1 ]
                            , [ 3, 2, 4, 5, 1 ]
                            , [ 3, 4, 1, 5, 2 ]
                            , [ 2, 5, 4, 3, 1 ]
                            , [ 3, 5, 1, 2, 4 ]
                            , [ 5, 1, 2, 3, 4 ]
                            , [ 4, 1, 3, 5, 2 ]
                            , [ 1, 2, 4, 3, 5 ]
                            , [ 4, 3, 5, 1, 2 ]
                            , [ 1, 5, 3, 4, 2 ]
                            , [ 1, 2, 4, 5, 3 ]
                            , [ 2, 3, 5, 1, 4 ]
                            , [ 4, 2, 5, 1, 3 ]
                            , [ 1, 2, 5, 3, 4 ]
                            , [ 2, 5, 1, 4, 3 ]
                            , [ 2, 5, 4, 1, 3 ]
                            , [ 4, 3, 5, 1, 2 ]
                            , [ 5, 1, 2, 3, 4 ]
                            ]

                        ballotWeight : BallotWeights
                        ballotWeight =
                            [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 ]

                        expected : List ( CandidatePosition, VoteWeight )
                        expected =
                            [ ( 1, 1 )
                            , ( 2, 2 )
                            , ( 5, 3 )
                            , ( 5, 4 )
                            , ( 3, 5 )
                            , ( 5, 6 )
                            , ( 3, 7 )
                            , ( 2, 8 )
                            , ( 2, 9 )
                            , ( 1, 10 )
                            , ( 4, 11 )
                            , ( 1, 12 )
                            , ( 1, 13 )
                            , ( 4, 14 )
                            , ( 4, 15 )
                            , ( 1, 16 )
                            , ( 3, 17 )
                            , ( 4, 18 )
                            , ( 4, 19 )
                            , ( 2, 20 )
                            ]
                    in
                    firstPreferences ballots ballotWeight
                        |> Expect.equal expected
            , test "works fine with ballots without first preference" <|
                \_ ->
                    let
                        ballots : Ballots
                        ballots =
                            [ [ 1, 2, 3, 4, 5 ]
                            , [ 2, 3, 4, 5, 0 ]
                            ]

                        ballotWeight : BallotWeights
                        ballotWeight =
                            [ 10, 20 ]

                        expected : List ( CandidatePosition, VoteWeight )
                        expected =
                            [ ( 1, 10 ), ( 0, 20 ) ]
                    in
                    firstPreferences ballots ballotWeight
                        |> Expect.equal expected
            ]
        , describe "countVotes function"
            [ test "works fine with some ballots" <|
                \_ ->
                    let
                        firstPrefs : List ( CandidatePosition, VoteWeight )
                        firstPrefs =
                            [ ( 2, 10 )
                            , ( 2, 20 )
                            , ( 3, 25 )
                            , ( 1, 5 )
                            ]

                        expected : Dict.Dict CandidatePosition VoteWeight
                        expected =
                            Dict.fromList [ ( 1, 5 ), ( 2, 30 ), ( 3, 25 ) ]
                    in
                    countVotes firstPrefs
                        |> Expect.equal expected
            ]
        , describe "calcQuota"
            [ test "works fine with some votes" <|
                \_ ->
                    let
                        remainingSeats : RemainingSeats
                        remainingSeats =
                            2

                        addOn : AddOn
                        addOn =
                            1

                        votes : Dict.Dict CandidatePosition VoteWeight
                        votes =
                            Dict.fromList
                                [ ( 1, 5 )
                                , ( 2, 30 )
                                , ( 3, 25 )
                                ]

                        expected : Quota
                        expected =
                            21
                    in
                    calcQuota remainingSeats addOn votes
                        |> Expect.equal expected
            , describe "works in edge cases"
                [ test "a" <|
                    \_ ->
                        calcQuota 1 1 (Dict.fromList [ ( 1, 1 ) ])
                            |> Expect.equal 1
                , test "b" <|
                    \_ ->
                        calcQuota 1 1 (Dict.fromList [ ( 1, 1 ), ( 2, 1 ) ])
                            |> Expect.equal 2
                , test "c" <|
                    \_ ->
                        calcQuota 1 1 (Dict.fromList [ ( 1, 1 ), ( 2, 1 ), ( 3, 1 ) ])
                            |> Expect.equal 2
                ]
            ]
        , describe "electOrReject function"
            [ test "works fine with some ballots" <|
                \_ ->
                    let
                        votes : Dict.Dict CandidatePosition VoteWeight
                        votes =
                            Dict.fromList
                                [ ( 1, 5 )
                                , ( 2, 30 )
                                , ( 3, 25 )
                                ]
                    in
                    electOrReject 21 votes
                        |> Expect.equal (IsElected 2)
            , test "works fine with tie breaking" <|
                \_ ->
                    let
                        votes : Dict.Dict CandidatePosition VoteWeight
                        votes =
                            Dict.fromList
                                [ ( 1, 12 )
                                , ( 2, 12 )
                                , ( 3, 6 )
                                ]
                    in
                    electOrReject 11 votes
                        |> Expect.equal (IsElected 27)
            , todo "works fine with one looser"
            , todo "works fine with tie breaking for loosers"
            ]
        ]
