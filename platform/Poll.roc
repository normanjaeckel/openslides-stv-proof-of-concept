interface Poll
    exposes [
        CandidateGroup,
        CandidateID,
        NumOfSeats,
        Poll,
        PollError,
        Vote,
    ]
    imports []

Poll : {
    seats : NumOfSeats,
    votes : List Vote,
    tieRank : List CandidateID,
}

CandidateID : U64

NumOfSeats : U64

Vote : List CandidateGroup
CandidateGroup : List CandidateID

PollError : [
    ZeroSeats,
    MoreSeatsThanCandidates,
    EqualSeatsThanCandidates,
    IdenticalTieRanks,
    InvalidVoteLength,
    EmptyVotes,
]
