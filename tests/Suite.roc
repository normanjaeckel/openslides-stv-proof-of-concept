interface Suite
    exposes [suite]
    imports []

suite = [
    test1,
    test2,
    test3,
    test4,
    test5,
    test6,
    errorTest1,
    errorTest2,
    errorTest3,
    errorTest4,
    errorTest5,
    errorTest6,
]

test1 =
    (
        "test 01 - only one winner because of many empty votes",
        { seats: 2, votes: [[1, 0, 0], [0, 0, 0], [0, 0, 0]], tieRank: [1, 2, 3] },
        Ok [0],
    )

test2 =
    (
        "test 02",
        { seats: 1, votes: [[1, 2, 3], [2, 3, 1], [3, 1, 2]], tieRank: [1, 2, 3] },
        Ok [2],
    )

test3 =
    (
        "test 03",
        {
            seats: 2,
            votes: [
                [4, 1, 3, 2],
                [2, 4, 1, 3],
                [1, 4, 2, 3],
                [1, 2, 4, 3],
                [1, 4, 3, 0],
                [3, 2, 4, 1],
                [3, 4, 1, 2],
                [3, 4, 1, 2],
                [4, 3, 2, 0],
                [2, 3, 4, 1],
            ],
            tieRank: [1, 2, 3, 4],
        },
        Ok [1, 2],
    )

test4 =
    (
        "test 04",
        {
            seats: 2,
            votes: [
                [4, 1, 3, 2],
                [2, 4, 1, 3],
                [2, 4, 1, 3],
                [1, 2, 4, 3],
                [1, 4, 0, 3],
                [3, 2, 4, 1],
                [3, 4, 1, 2],
                [3, 4, 1, 2],
                [4, 3, 2, 0],
                [2, 3, 4, 1],
            ],
            tieRank: [1, 2, 3, 4],
        },
        Ok [1, 0],
    )

test5 =
    (
        "test 05",
        {
            seats: 2,
            votes: [
                [0, 0, 0, 0],
                [0, 4, 0, 0],
                [0, 4, 0, 0],
                [0, 2, 0, 0],
                [0, 4, 0, 0],
                [0, 0, 0, 0],
                [0, 4, 0, 0],
                [0, 4, 0, 0],
                [0, 0, 0, 0],
                [0, 0, 0, 0],
            ],
            tieRank: [1, 2, 3, 4],
        },
        Ok [1],
    )

test6 =
    (
        "test 06",
        {
            seats: 2,
            votes: [
                [0, 0, 0, 0],
                [0, 4, 0, 0],
                [0, 4, 0, 0],
                [0, 2, 0, 0],
                [0, 4, 0, 0],
                [0, 0, 0, 0],
                [1, 4, 0, 0],
                [0, 4, 0, 1],
                [1, 0, 0, 0],
                [0, 0, 0, 1],
            ],
            tieRank: [1, 2, 3, 4],
        },
        Ok [1, 3],
    )

errorTest1 =
    (
        "test ZeroSeats error",
        { seats: 0, votes: [[1, 2], [1, 2], [1, 2]], tieRank: [1, 2] },
        Err ZeroSeats,
    )

errorTest2 =
    (
        "test MoreSeatsThanCandidates error",
        { seats: 3, votes: [[1, 2], [1, 2], [1, 2]], tieRank: [1, 2] },
        Err MoreSeatsThanCandidates,
    )

errorTest3 =
    (
        "test EqualSeatsThanCandidates error",
        { seats: 2, votes: [[1, 2], [1, 2], [1, 2]], tieRank: [1, 2] },
        Err EqualSeatsThanCandidates,
    )

errorTest4 =
    (
        "test IdenticalTieRanks error",
        { seats: 1, votes: [[1, 2], [1, 2], [1, 2]], tieRank: [1, 1] },
        Err IdenticalTieRanks,
    )

errorTest5 =
    (
        "test InvalidVoteLength error",
        { seats: 1, votes: [[1, 2], [1, 2], [1, 2, 3]], tieRank: [1, 2] },
        Err InvalidVoteLength,
    )

errorTest6 =
    (
        "test EmptyVotes error",
        { seats: 1, votes: [], tieRank: [1, 2] },
        Err EmptyVotes,
    )
