interface Poll
    exposes [
        CandidateIndex,
        NumOfSeats,
        Poll,
        PollError,
        Preference,
        Vote,
    ]
    imports []

Poll : {
    seats : NumOfSeats,
    votes : List Vote,
    tieRank : List U64,
}

NumOfSeats : U64

Vote : List Preference

Preference : U64

CandidateIndex : U64

PollError : [
    ZeroSeats,
    MoreSeatsThanCandidates,
    EqualSeatsThanCandidates,
    IdenticalTieRanks,
    InvalidVoteLength,
    EmptyVotes,
]
