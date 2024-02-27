interface Poll
    exposes [
        Poll,
        PollError,
        Preference,
        Vote,
    ]
    imports []

Poll : {
    seats : U32,
    votes : List Vote,
    tieRank : List U32,
}

Vote : List Preference

Preference : U32

PollError : [
    ZeroSeats,
    MoreSeatsThanCandidates,
    IdenticalTieRanks,
    InvalidVoteLength,
]
