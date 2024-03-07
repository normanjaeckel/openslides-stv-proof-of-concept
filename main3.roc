app "single-transferable-vote"
    packages {
        pf: "./platform/main.roc",
        tests: "./tests/main.roc",
    }
    imports [
        pf.Poll.{ CandidateGroup, CandidateID, NumOfSeats, Poll, PollError, Vote },
        tests.Suite,
    ]
    provides [main] to pf

main : Poll -> Result (List CandidateID) PollError
main = \poll ->
    poll
    |> validate
    |> Result.map singleTransferableVote

# expect
#     failedTests =
#         List.walk
#             Suite.suite
#             []
#             \state, (name, poll, expected) ->
#                 got = main poll
#                 if got == expected then
#                     state
#                 else
#                     List.append state { aname: name, got: got, expected: expected }

#     List.len failedTests == 0

validate : Poll -> Result Poll PollError
validate = \poll ->
    Ok poll
# |> Result.try checkSeats
# |> Result.try checkTieRank
# |> Result.try checkVotes

# checkSeats = \poll ->
#     if poll.seats == 0 then
#         Err ZeroSeats
#     else
#         numOfCandidates = List.len poll.tieRank
#         if poll.seats > numOfCandidates then
#             Err MoreSeatsThanCandidates
#         else if poll.seats == numOfCandidates then
#             Err EqualSeatsThanCandidates
#         else
#             Ok poll

# checkTieRank = \poll ->
#     if Set.fromList poll.tieRank |> Set.len != List.len poll.tieRank then
#         Err IdenticalTieRanks
#     else
#         Ok poll

# checkVotes = \poll ->
#     numOfCandidates = List.len poll.tieRank
#     if !(poll.votes |> List.all \vote -> List.len vote == numOfCandidates) then
#         Err InvalidVoteLength
#     else if poll.votes |> List.all \vote -> (List.max vote |> Result.withDefault 0) == 0 then
#         Err EmptyVotes
#     else
#         Ok poll

# expect
#     poll = { seats: 2, votes: [[1, 2, 3], [1, 2, 3], [1, 2, 3]], tieRank: [1, 2, 3] }
#     Result.isOk (validate poll)

# expect
#     poll = { seats: 0, tieRank: [1, 2, 3], votes: [] }
#     validate poll == Err ZeroSeats

# expect
#     poll = { seats: 4, tieRank: [1, 2, 3], votes: [] }
#     validate poll == Err MoreSeatsThanCandidates

# expect
#     poll = { seats: 3, tieRank: [1, 2, 3], votes: [] }
#     validate poll == Err EqualSeatsThanCandidates

# expect
#     poll = { seats: 2, tieRank: [1, 2, 2], votes: [] }
#     validate poll == Err IdenticalTieRanks

# expect
#     poll = { seats: 2, tieRank: [1, 2, 3], votes: [[1, 2, 3], [1, 2]] }
#     validate poll == Err InvalidVoteLength

# expect
#     poll = { seats: 2, tieRank: [1, 2, 3], votes: [[1, 2, 3], [1, 2, 3, 4]] }
#     validate poll == Err InvalidVoteLength

# expect
#     poll = { seats: 2, tieRank: [1, 2, 3], votes: [] }
#     validate poll == Err EmptyVotes

# expect
#     poll = { seats: 2, tieRank: [1, 2, 3], votes: [[0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]] }
#     validate poll == Err EmptyVotes

PollData : {
    tieRank : List CandidateID,
    seats : NumOfSeats,
    electedCandidates : List CandidateID,
    eliminatedCandidates : List CandidateID,
    votes : List Vote,
    voteWeights : List VoteWeight,
}

VoteWeight : U64

initialPollData : Poll -> PollData
initialPollData = \poll -> {
    tieRank: poll.tieRank,
    seats: poll.seats,
    electedCandidates: [],
    eliminatedCandidates: [],
    votes: poll.votes,
    voteWeights: List.repeat 1_000_000 (List.len poll.votes),
}

remainingSeats : PollData -> U64
remainingSeats = \pd ->
    pd.seats - List.len pd.electedCandidates

singleTransferableVote : Poll -> List CandidateID
singleTransferableVote = \poll ->
    poll
    |> initialPollData
    |> singleTransferableVoteHelper

singleTransferableVoteHelper = \pd ->
    when round pd is
        Done candidates -> candidates
        Continue newPD -> singleTransferableVoteHelper newPD

round : PollData -> [Done (List CandidateID), Continue PollData]
round = \pd ->
    ignore = List.concat pd.electedCandidates pd.eliminatedCandidates
    highestCandidates = getHighest pd.votes ignore
    countedVotes = countVotes pd.tieRank pd.voteWeights highestCandidates
    (votedCandidate, numberOfVotes, lowestCandidate) = getVotedCandidate countedVotes pd.tieRank ignore

    if numberOfVotes == 0 then
        Done pd.electedCandidates
    else
        quota = ((List.sum countedVotes) // (remainingSeats pd + 1)) + 1

        if numberOfVotes >= quota then
            electedCandidates = List.append pd.electedCandidates votedCandidate

            if List.len electedCandidates == pd.seats then
                Done electedCandidates
            else
                Continue
                    { pd &
                        electedCandidates: electedCandidates,
                        voteWeights: updateVoteWeights pd.voteWeights highestCandidates votedCandidate numberOfVotes quota,
                    }
        else
            Continue
                { pd &
                    eliminatedCandidates: List.append pd.eliminatedCandidates lowestCandidate,
                }

# expect
#     got = round {
#         seats: 1,
#         tieRank: [1, 2, 3],
#         electedCandidates: [],
#         eliminatedCandidates: [],
#         votes: [[[1], [2], [3]], [[1], [2], [3]], [[1], [2], [3]]],
#         voteWeights: [10, 10, 10],
#     }
#     got
#     == Done [2]

# expect
#     got = round {
#         seats: 1,
#         tieRank: [1, 2, 3],
#         electedCandidates: [],
#         eliminatedCandidates: [],
#         votes: [[1, 2, 3], [2, 3, 1], [3, 1, 2]],
#         voteWeights: [10, 10, 10],
#     }

#     got
#     == Continue {
#         seats: 1,
#         tieRank: [1, 2, 3],
#         electedCandidates: [],
#         eliminatedCandidates: [0],
#         sortedVotes: sortVotes [[1, 2, 3], [2, 3, 1], [3, 1, 2]],
#         voteWeights: [10, 10, 10],
#     }

# expect
#     got = round {
#         seats: 1,
#         tieRank: [1, 2, 3],
#         electedCandidates: [],
#         eliminatedCandidates: [0],
#         sortedVotes: sortVotes [[1, 2, 3], [2, 3, 1], [3, 1, 2]],
#         voteWeights: [10, 10, 10],
#     }
#     got
#     == Done [2]

getHighest : List Vote, List CandidateID -> Vote
getHighest = \votes, ignore ->
    List.map
        votes
        \vote ->
            when List.findFirst vote \v -> listHasNoIntersect v ignore is
                Ok v -> v
                Err NotFound -> []

# expect
#     sortedVotes = sortVotes [[1, 2, 3], [1, 2, 2], [4, 1, 0], [0, 0, 0]]
#     got = getHighest sortedVotes []
#     got == [[2], [2, 1], [0], []]

countVotes : List U64, List U64, Vote -> List U64
countVotes = \tieRank, voteWeights, votes ->
    List.map2 votes voteWeights (\v, w -> (v, w))
    |> List.walk
        (List.repeat 0 (List.len tieRank))
        \state, (candidateIdxList, weight) ->
            if List.len candidateIdxList == 0 then
                state
            else
                w = weight // List.len candidateIdxList
                List.walk candidateIdxList state \state2, candidateIndex ->
                    List.update state2 candidateIndex \v -> v + w

# expect
#     ignore = []
#     highestCandidates = getHighest (sortVotes [[1, 1, 1]]) ignore
#     got = countVotes [1, 2, 3] [1_000_000, 1_000_000, 1_000_000] highestCandidates
#     got == [333_333, 333_333, 333_333]

getVotedCandidate : List U64, List U64, List U64 -> (CandidateID, U64, CandidateID)
getVotedCandidate = \counted, tieRank, ignore ->
    { highestValue, highestIndex, lowestIndex } =
        List.walkWithIndex
            counted
            { highestValue: 0, lowestValue: Num.maxU64, highestIndex: 0, lowestIndex: 0 }
            \state, voteCount, index ->
                if List.contains ignore index then
                    state
                else
                    state2 =
                        if voteCount > state.highestValue then
                            { state & highestValue: voteCount, highestIndex: index }
                        else if voteCount == state.highestValue then
                            x = List.get tieRank index |> Result.withDefault 0
                            y = List.get tieRank state.highestIndex |> Result.withDefault 0

                            if x > y then
                                { state & highestValue: voteCount, highestIndex: index }
                            else
                                state
                        else
                            state

                    if voteCount < state2.lowestValue then
                        { state2 & lowestValue: voteCount, lowestIndex: index }
                    else if voteCount == state2.lowestValue then
                        x = List.get tieRank index |> Result.withDefault 0
                        y = List.get tieRank state2.lowestIndex |> Result.withDefault 0

                        if x < y then
                            { state2 & lowestValue: voteCount, lowestIndex: index }
                        else
                            state2
                    else
                        state2

    (highestIndex, highestValue, lowestIndex)

updateVoteWeights : List U64, Vote, CandidateID, U64, U64 -> List U64
updateVoteWeights = \voteWeights, vote, candidateIndex, numberOfVotes, quota ->
    surplus = numberOfVotes - quota
    numOfPrefs = whoHasVoted vote candidateIndex

    List.map2
        numOfPrefs
        voteWeights
        \numOfPref, voteWeight ->
            if numOfPref == 0 then
                voteWeight
            else
                x = (voteWeight // numOfPref)
                voteWeight - x + (x * surplus // numberOfVotes)

whoHasVoted : Vote, CandidateID -> List U64
whoHasVoted = \vote, winner ->
    List.map
        vote
        \v ->
            if List.contains v winner then
                List.len vote
            else
                0

listHasNoIntersect : List a, List a -> Bool where a implements Eq
listHasNoIntersect = \aList, bList ->
    List.all aList \e -> !(List.contains bList e)
