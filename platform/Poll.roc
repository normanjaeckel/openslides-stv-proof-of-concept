interface Poll
    exposes [
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
    tieRank : List U32,
}

NumOfSeats : U32

Vote : List Preference

Preference : U32

PollError : [
    ZeroSeats,
    MoreSeatsThanCandidates,
    IdenticalTieRanks,
    InvalidVoteLength,
]
