interface Poll
    exposes [
        CandidateGroup,
        CandidateID,
        NumOfSeats,
        Poll,
        PollError,
        TieRank,
        Vote,
    ]
    imports []

Poll : {
    seats : NumOfSeats,
    votes : List Vote,
    tieRank : TieRank,
}

TieRank : Dict CandidateID U64

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
