platform "stv-wasm"
    requires {} { main : Poll -> Result (List CandidateIndex) PollError }
    exposes []
    packages {}
    imports [Poll.{ CandidateIndex, Poll, PollError }]
    provides [mainForHost]

mainForHost : Poll32 -> ReturnValue
mainForHost = \poll32 ->
    poll = pollToU64 poll32
    when main poll is
        Ok winners ->
            winners
            |> List.map Num.toU32
            |> List.prepend (List.len winners |> Num.toU32)
            |> List.prepend 0

        Err err ->
            when err is
                ZeroSeats -> [10]
                MoreSeatsThanCandidates -> [11]
                EqualSeatsThanCandidates -> [12]
                IdenticalTieRanks -> [13]
                InvalidVoteLength -> [14]
                EmptyVotes -> [15]

## The return value is a list of numbers.
## The first number is a status code.
##
## Status code 0 means "Success".
## In this case the second number is the number of elected candidates.
## The following numbers are the indexes of these candidates.
##
## Example: [0, 3, 1, 9, 7] means the the candidates with index 1, 9 and 7 are elected.
##
## Status code 10 or higher means "Error". See the implementation above for the respective error codes.
ReturnValue : List U32

Poll32 : {
    seats : U32,
    votes : List (List U32),
    tieRank : List U32,
}

pollToU64 : Poll32 -> Poll
pollToU64 = \poll32 -> {
    seats: Num.toU64 poll32.seats,
    votes: poll32.votes |> List.map \vote -> vote |> List.map Num.toU64,
    tieRank: poll32.tieRank |> List.map Num.toU64,
}
