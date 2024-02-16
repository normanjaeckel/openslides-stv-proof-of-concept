module MainTest exposing (suite)

import Dict
import Expect
import Main
    exposing
        ( AddOn
        , BallotsWithWeight
        , CandidatePosition
        , Elected
        , ElectedOrRejected(..)
        , Poll
        , PollData
        , Quota
        , RemainingSeats
        , VoteWeight
        , calcQuota
        , countVotes
        , electOrReject
        , recomputeWeights
        , runOneRound
        , runSTVAlgorithm
        , shiftBallots
        , weightedFirstPreferences
        )
import Test exposing (..)


suite : Test
suite =
    describe "The"
        [ describe "runSTVAlgorithm function"
            [ test "works fine with some poll data" <|
                \_ ->
                    let
                        poll : Poll
                        poll =
                            { candidates = [ "John", "Anna", "Eli" ]
                            , seats = 2
                            , ballots =
                                [ [ 3, 1, 2 ]
                                , [ 3, 1, 2 ]
                                , [ 3, 2, 1 ]
                                , [ 1, 3, 2 ]
                                ]
                            }

                        expected : Elected
                        expected =
                            [ 2, 3 ]
                    in
                    runSTVAlgorithm poll |> Expect.equal expected
            ]
        , describe "runOneRound function"
            [ test "works fine with some poll data" <|
                \_ ->
                    let
                        pollData : PollData
                        pollData =
                            { numOfCandidates = 5
                            , ballots =
                                [ ( [ 1, 2, 3, 4, 5 ], 1200 )
                                , ( [ 1, 2, 3, 4, 5 ], 1200 )
                                , ( [ 1, 2, 3, 4, 5 ], 1200 )
                                , ( [ 5, 1, 2, 3, 4 ], 1200 )
                                , ( [ 5, 1, 2, 3, 4 ], 1200 )
                                , ( [ 3, 4, 1, 2, 5 ], 1200 )
                                ]
                            , remainingSeats = 2
                            , elected = []
                            , rejected = []
                            }

                        expected : PollData
                        expected =
                            { numOfCandidates = 5
                            , ballots =
                                [ ( [ 0, 1, 2, 3, 4 ], 399 )
                                , ( [ 0, 1, 2, 3, 4 ], 399 )
                                , ( [ 0, 1, 2, 3, 4 ], 399 )
                                , ( [ 0, 1, 2, 3, 4 ], 1200 )
                                , ( [ 0, 1, 2, 3, 4 ], 1200 )
                                , ( [ 0, 3, 1, 2, 4 ], 1200 )
                                ]
                            , remainingSeats = 1
                            , elected = [ 1 ]
                            , rejected = []
                            }
                    in
                    runOneRound pollData |> Expect.equal expected
            , test "works fine with some poll data with equal preferences" <|
                \_ ->
                    let
                        pollData : PollData
                        pollData =
                            { numOfCandidates = 5
                            , ballots =
                                [ ( [ 1, 2, 3, 4, 5 ], 1200000 )
                                , ( [ 3, 1, 1, 4, 5 ], 1200000 )
                                , ( [ 3, 1, 4, 1, 5 ], 1200000 )
                                , ( [ 3, 1, 2, 5, 4 ], 1200000 )
                                , ( [ 5, 1, 2, 3, 4 ], 1200000 )
                                , ( [ 3, 4, 1, 2, 5 ], 1200000 )
                                ]
                            , remainingSeats = 2
                            , elected = []
                            , rejected = []
                            }

                        expected : PollData
                        expected =
                            { numOfCandidates = 5
                            , ballots =
                                [ ( [ 1, 0, 2, 3, 4 ], 1200000 )
                                , ( [ 2, 0, 1, 3, 4 ], 799999 )
                                , ( [ 2, 0, 3, 1, 4 ], 799999 )
                                , ( [ 2, 0, 1, 4, 3 ], 399999 )
                                , ( [ 4, 0, 1, 2, 3 ], 399999 )
                                , ( [ 3, 0, 1, 2, 4 ], 1200000 )
                                ]
                            , remainingSeats = 1
                            , elected = [ 2 ]
                            , rejected = []
                            }
                    in
                    runOneRound pollData |> Expect.equal expected
            , test "works fine with some poll data with rejection" <|
                \_ ->
                    let
                        pollData : PollData
                        pollData =
                            { numOfCandidates = 5
                            , ballots =
                                [ ( [ 1, 2, 3, 4, 5 ], 1200000 )
                                , ( [ 3, 1, 1, 4, 5 ], 1200000 )
                                , ( [ 3, 1, 4, 1, 5 ], 1200000 )
                                , ( [ 3, 1, 2, 5, 4 ], 1200000 )
                                , ( [ 5, 2, 1, 3, 4 ], 1200000 )
                                , ( [ 1, 4, 3, 2, 5 ], 1200000 )
                                ]
                            , remainingSeats = 2
                            , elected = []
                            , rejected = []
                            }

                        expected : PollData
                        expected =
                            { numOfCandidates = 5
                            , ballots =
                                [ ( [ 1, 2, 3, 4, 0 ], 1200000 )
                                , ( [ 3, 1, 1, 4, 0 ], 1200000 )
                                , ( [ 3, 1, 4, 1, 0 ], 1200000 )
                                , ( [ 3, 1, 2, 4, 0 ], 1200000 )
                                , ( [ 4, 2, 1, 3, 0 ], 1200000 )
                                , ( [ 1, 4, 3, 2, 0 ], 1200000 )
                                ]
                            , remainingSeats = 2
                            , elected = []
                            , rejected = [ 5 ]
                            }
                    in
                    runOneRound pollData |> Expect.equal expected
            , test "works fine with some poll data with rejection with votes" <|
                \_ ->
                    let
                        pollData : PollData
                        pollData =
                            { numOfCandidates = 5
                            , ballots =
                                [ ( [ 1, 2, 3, 4, 5 ], 1200000 )
                                , ( [ 3, 1, 1, 4, 5 ], 1200000 )
                                , ( [ 3, 1, 4, 1, 5 ], 1200000 )
                                , ( [ 3, 1, 2, 5, 4 ], 1200000 )
                                , ( [ 5, 4, 1, 1, 1 ], 1200000 )
                                , ( [ 1, 4, 3, 2, 5 ], 1200000 )
                                ]
                            , remainingSeats = 2
                            , elected = []
                            , rejected = []
                            }

                        expected : PollData
                        expected =
                            { numOfCandidates = 5
                            , ballots =
                                [ ( [ 1, 2, 3, 4, 0 ], 1200000 )
                                , ( [ 3, 1, 1, 4, 0 ], 1200000 )
                                , ( [ 3, 1, 4, 1, 0 ], 1200000 )
                                , ( [ 3, 1, 2, 4, 0 ], 1200000 )
                                , ( [ 4, 3, 1, 1, 0 ], 1200000 )
                                , ( [ 1, 4, 3, 2, 0 ], 1200000 )
                                ]
                            , remainingSeats = 2
                            , elected = []
                            , rejected = [ 5 ]
                            }
                    in
                    runOneRound pollData |> Expect.equal expected
            ]
        , describe "weightedFirstPreferences function"
            [ test "works fine with some ballots" <|
                \_ ->
                    let
                        ballots : BallotsWithWeight
                        ballots =
                            [ ( [ 1, 2, 3, 4, 5 ], 10 )
                            , ( [ 2, 1, 3, 5, 4 ], 20 )
                            , ( [ 1, 3, 2, 4, 5 ], 30 )
                            ]

                        expected : List (List VoteWeight)
                        expected =
                            [ [ 10, 0, 0, 0, 0 ]
                            , [ 0, 20, 0, 0, 0 ]
                            , [ 30, 0, 0, 0, 0 ]
                            ]
                    in
                    weightedFirstPreferences ballots
                        |> Expect.equal expected
            , test "works fine with many ballots" <|
                \_ ->
                    let
                        ballots : BallotsWithWeight
                        ballots =
                            [ ( [ 1, 5, 2, 3, 4 ], 1 )
                            , ( [ 2, 1, 3, 5, 4 ], 2 )
                            , ( [ 4, 2, 3, 5, 1 ], 3 )
                            , ( [ 3, 2, 4, 5, 1 ], 4 )
                            , ( [ 3, 4, 1, 5, 2 ], 5 )
                            , ( [ 2, 5, 4, 3, 1 ], 6 )
                            , ( [ 3, 5, 1, 2, 4 ], 7 )
                            , ( [ 5, 1, 2, 3, 4 ], 8 )
                            , ( [ 4, 1, 3, 5, 2 ], 9 )
                            , ( [ 1, 2, 4, 3, 5 ], 10 )
                            , ( [ 4, 3, 5, 1, 2 ], 11 )
                            , ( [ 1, 5, 3, 4, 2 ], 12 )
                            , ( [ 1, 2, 4, 5, 3 ], 13 )
                            , ( [ 2, 3, 5, 1, 4 ], 14 )
                            , ( [ 4, 2, 5, 1, 3 ], 15 )
                            , ( [ 1, 2, 5, 3, 4 ], 16 )
                            , ( [ 2, 5, 1, 4, 3 ], 17 )
                            , ( [ 2, 5, 4, 1, 3 ], 18 )
                            , ( [ 4, 3, 5, 1, 2 ], 19 )
                            , ( [ 5, 1, 2, 3, 4 ], 20 )
                            ]

                        expected : List (List VoteWeight)
                        expected =
                            [ [ 1, 0, 0, 0, 0 ]
                            , [ 0, 2, 0, 0, 0 ]
                            , [ 0, 0, 0, 0, 3 ]
                            , [ 0, 0, 0, 0, 4 ]
                            , [ 0, 0, 5, 0, 0 ]
                            , [ 0, 0, 0, 0, 6 ]
                            , [ 0, 0, 7, 0, 0 ]
                            , [ 0, 8, 0, 0, 0 ]
                            , [ 0, 9, 0, 0, 0 ]
                            , [ 10, 0, 0, 0, 0 ]
                            , [ 0, 0, 0, 11, 0 ]
                            , [ 12, 0, 0, 0, 0 ]
                            , [ 13, 0, 0, 0, 0 ]
                            , [ 0, 0, 0, 14, 0 ]
                            , [ 0, 0, 0, 15, 0 ]
                            , [ 16, 0, 0, 0, 0 ]
                            , [ 0, 0, 17, 0, 0 ]
                            , [ 0, 0, 0, 18, 0 ]
                            , [ 0, 0, 0, 19, 0 ]
                            , [ 0, 20, 0, 0, 0 ]
                            ]
                    in
                    weightedFirstPreferences ballots
                        |> Expect.equal expected
            , test "works fine with ballots without first preference" <|
                \_ ->
                    let
                        ballots : BallotsWithWeight
                        ballots =
                            [ ( [ 1, 2, 3, 4, 5 ], 10 )
                            , ( [ 2, 3, 4, 5, 0 ], 20 )
                            ]

                        expected : List (List VoteWeight)
                        expected =
                            [ [ 10, 0, 0, 0, 0 ]
                            , [ 0, 0, 0, 0, 0 ]
                            ]
                    in
                    weightedFirstPreferences ballots
                        |> Expect.equal expected
            , test "works fine with ballots without equal preference" <|
                \_ ->
                    let
                        ballots : BallotsWithWeight
                        ballots =
                            [ ( [ 1, 1, 3, 4, 5 ], 10 )
                            , ( [ 1, 2, 3, 3, 0 ], 20 )
                            ]

                        expected : List (List VoteWeight)
                        expected =
                            [ [ 5, 5, 0, 0, 0 ]
                            , [ 20, 0, 0, 0, 0 ]
                            ]
                    in
                    weightedFirstPreferences ballots
                        |> Expect.equal expected
            ]
        , describe "countVotes function"
            [ test "works fine with some ballots" <|
                \_ ->
                    let
                        weightedFirstPrefs : List (List VoteWeight)
                        weightedFirstPrefs =
                            [ [ 10, 0, 0, 0, 0 ]
                            , [ 0, 20, 0, 0, 0 ]
                            , [ 5, 0, 0, 0, 0 ]
                            ]

                        expected : Dict.Dict CandidatePosition VoteWeight
                        expected =
                            Dict.fromList [ ( 1, 15 ), ( 2, 20 ), ( 3, 0 ), ( 4, 0 ), ( 5, 0 ) ]
                    in
                    countVotes 5 weightedFirstPrefs
                        |> Expect.equal expected
            , test "works fine with ballots without equal preference" <|
                \_ ->
                    let
                        weightedFirstPrefs : List (List VoteWeight)
                        weightedFirstPrefs =
                            [ [ 5, 5, 0, 0, 0 ]
                            , [ 20, 0, 0, 0, 0 ]
                            , [ 0, 10, 0, 0, 0 ]
                            ]

                        expected : Dict.Dict CandidatePosition VoteWeight
                        expected =
                            Dict.fromList [ ( 1, 25 ), ( 2, 15 ), ( 3, 0 ), ( 4, 0 ), ( 5, 0 ) ]
                    in
                    countVotes 5 weightedFirstPrefs
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
                        |> Expect.equal (IsElected 2 30)
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
                        |> Expect.equal (IsElected 0 0)
            , test "works fine with one looser" <|
                \_ ->
                    let
                        votes : Dict.Dict CandidatePosition VoteWeight
                        votes =
                            Dict.fromList
                                [ ( 1, 10 )
                                , ( 2, 3 )
                                , ( 3, 10 )
                                , ( 4, 7 )
                                ]
                    in
                    electOrReject 11 votes |> Expect.equal (IsRejected 2)
            , todo "works fine with tie breaking for loosers"
            ]
        , describe "recomputeWeights function"
            [ test "works fine with some ballots" <|
                \_ ->
                    let
                        ballots : BallotsWithWeight
                        ballots =
                            [ ( [ 2, 1, 3, 4, 5 ], 15 )
                            , ( [ 1, 2, 3, 4, 5 ], 10 )
                            , ( [ 1, 1, 3, 4, 5 ], 20 )
                            ]

                        wFP : List (List VoteWeight)
                        wFP =
                            [ [ 0, 15, 0, 0, 0 ]
                            , [ 10, 0, 0, 0, 0 ]
                            , [ 10, 10, 0, 0, 0 ]
                            ]

                        expected : BallotsWithWeight
                        expected =
                            [ ( [ 2, 1, 3, 4, 5 ], 5 )
                            , ( [ 1, 2, 3, 4, 5 ], 10 )
                            , ( [ 1, 1, 3, 4, 5 ], 13 )
                            ]
                    in
                    recomputeWeights 2 16 25 wFP ballots |> Expect.equal expected
            , test "works fine with some ballots with precision 1000" <|
                \_ ->
                    let
                        ballots : BallotsWithWeight
                        ballots =
                            [ ( [ 2, 1, 3, 4, 5 ], 15000 )
                            , ( [ 1, 2, 3, 4, 5 ], 10000 )
                            , ( [ 1, 1, 3, 4, 5 ], 20000 )
                            ]

                        wFP : List (List VoteWeight)
                        wFP =
                            [ [ 0, 15000, 0, 0, 0 ]
                            , [ 10000, 0, 0, 0, 0 ]
                            , [ 10000, 10000, 0, 0, 0 ]
                            ]

                        expected : BallotsWithWeight
                        expected =
                            [ ( [ 2, 1, 3, 4, 5 ], 5400 )
                            , ( [ 1, 2, 3, 4, 5 ], 10000 )
                            , ( [ 1, 1, 3, 4, 5 ], 13600 )
                            ]
                    in
                    recomputeWeights 2 16 25 wFP ballots |> Expect.equal expected
            ]
        , describe "shiftBallots function"
            [ test "works fine with some ballots" <|
                \_ ->
                    let
                        ballots : BallotsWithWeight
                        ballots =
                            [ ( [ 1, 2, 3 ], 1 )
                            , ( [ 0, 0, 0 ], 1 )
                            , ( [ 0, 1, 2 ], 1 )
                            , ( [ 2, 2, 1 ], 1 )
                            , ( [ 2, 2, 4 ], 1 )
                            ]

                        expected : BallotsWithWeight
                        expected =
                            [ ( [ 1, 0, 2 ], 1 )
                            , ( [ 0, 0, 0 ], 1 )
                            , ( [ 0, 0, 1 ], 1 )
                            , ( [ 2, 0, 1 ], 1 )
                            , ( [ 2, 0, 3 ], 1 )
                            ]
                    in
                    shiftBallots 2 ballots |> Expect.equal expected
            ]
        ]
