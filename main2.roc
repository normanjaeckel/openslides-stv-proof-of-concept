app "single-transferable-vote"
    packages { pf: "./platform/main.roc" }
    imports [pf.Poll.{ CandidateIndex, NumOfSeats, Poll, PollError, Preference, Vote }]
    provides [main] to pf

main : Poll -> Result (List CandidateIndex) PollError
main = \poll ->
    poll
    |> validate
    |> Result.map singleTransferableVote

# validate and all sub-functions are mainly copied from Norman.
validate : Poll -> Result Poll PollError
validate = \poll ->
    poll
    |> checkSeats
    |> Result.try checkTieRank
    |> Result.try checkVotes

checkSeats = \poll ->
    if poll.seats == 0 then
        Err ZeroSeats
    else if poll.seats > List.len poll.tieRank then
        Err MoreSeatsThanCandidates
    else
        Ok poll

checkTieRank = \poll ->
    if Set.fromList poll.tieRank |> Set.len != List.len poll.tieRank then
        Err IdenticalTieRanks
    else
        Ok poll

checkVotes = \poll ->
    numOfCandidates = List.len poll.tieRank
    if !(poll.votes |> List.all \vote -> List.len vote == numOfCandidates) then
        Err InvalidVoteLength
    else
        Ok poll

PollData : {
    poll : Poll,
    electedCandidates : List CandidateIndex,
    eliminatedCandidates : List CandidateIndex,
    voteWeights : List VoteWeight,
}

VoteWeight : U64

initialPollData : Poll -> PollData
initialPollData = \poll -> {
    poll: poll,
    electedCandidates: [],
    eliminatedCandidates: [],
    voteWeights: List.repeat 1_000_000 (List.len poll.votes),
}

remainingSeats : PollData -> U64
remainingSeats = \pd ->
    pd.poll.seats - List.len pd.electedCandidates

ignoreIndex : PollData -> List CandidateIndex
ignoreIndex = \pd ->
    List.concat pd.electedCandidates pd.eliminatedCandidates

singleTransferableVote : Poll -> List CandidateIndex
singleTransferableVote = \poll ->
    poll
    |> initialPollData
    |> singleTransferableVoteHelper

singleTransferableVoteHelper = \pd ->
    when round pd is
        Done candidates -> candidates
        Continue newPD -> singleTransferableVoteHelper newPD

round : PollData -> [Done (List CandidateIndex), Continue PollData]
round = \pd ->
    if List.len pd.electedCandidates == pd.poll.seats then
        Done pd.electedCandidates
    else
        countedVotes = countVotes pd
        (votedCandidate, numberOfVotes, lowestCandidate) = getVotedCandidate pd countedVotes

        if numberOfVotes == 0 then
            Done pd.electedCandidates
        else
            quota = ((List.sum countedVotes) // (remainingSeats pd + 1)) + 1

            if numberOfVotes >= quota then
                Continue
                    { pd &
                        electedCandidates: List.append pd.electedCandidates votedCandidate,
                        voteWeights: updateVoteWeights pd votedCandidate numberOfVotes quota,
                    }
            else
                Continue
                    { pd &
                        eliminatedCandidates: List.append pd.eliminatedCandidates lowestCandidate,
                    }

expect
    poll = {
        seats: 1,
        votes: [[1, 2, 3], [1, 2, 3], [1, 2, 3]],
        tieRank: [1, 2, 3],
    }

    got = round {
        poll: poll,
        electedCandidates: [],
        eliminatedCandidates: [],
        voteWeights: [10, 10, 10],
    }
    got
    == Continue {
        poll: poll,
        electedCandidates: [2],
        eliminatedCandidates: [],
        voteWeights: [4, 4, 4],
    }

expect
    poll = {
        seats: 1,
        votes: [[1, 2, 3], [2, 3, 1], [3, 1, 2]],
        tieRank: [1, 2, 3],
    }

    got = round {
        poll: poll,
        electedCandidates: [],
        eliminatedCandidates: [],
        voteWeights: [10, 10, 10],
    }
    got
    == Continue {
        poll: poll,
        electedCandidates: [],
        eliminatedCandidates: [0],
        voteWeights: [10, 10, 10],
    }

expect
    poll = {
        seats: 1,
        votes: [[1, 2, 3], [2, 3, 1], [3, 1, 2]],
        tieRank: [1, 2, 3],
    }

    got = round {
        poll: poll,
        electedCandidates: [],
        eliminatedCandidates: [0],
        voteWeights: [10, 10, 10],
    }
    got
    == Continue {
        poll: poll,
        electedCandidates: [2],
        eliminatedCandidates: [0],
        voteWeights: [2, 10, 2],
    }

expect
    poll = {
        seats: 1,
        votes: [[1, 2, 3], [2, 3, 1], [3, 1, 2]],
        tieRank: [1, 2, 3],
    }

    got = round {
        poll: poll,
        electedCandidates: [2],
        eliminatedCandidates: [0],
        voteWeights: [2, 10, 2],
    }
    got
    == Done [2]

getVotedCandidate : PollData, List U64 -> (CandidateIndex, U64, CandidateIndex)
getVotedCandidate = \pd, counted ->
    ignore = ignoreIndex pd
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
                            x = List.get pd.poll.tieRank index |> Result.withDefault 0
                            y = List.get pd.poll.tieRank state.highestIndex |> Result.withDefault 0

                            if x > y then
                                { state & highestValue: voteCount, highestIndex: index }
                            else
                                state
                        else
                            state

                    if voteCount < state2.lowestValue then
                        { state2 & lowestValue: voteCount, lowestIndex: index }
                    else if voteCount == state2.lowestValue then
                        x = List.get pd.poll.tieRank index |> Result.withDefault 0
                        y = List.get pd.poll.tieRank state2.lowestIndex |> Result.withDefault 0

                        if x < y then
                            { state2 & lowestValue: voteCount, lowestIndex: index }
                        else
                            state
                    else
                        state2

    (highestIndex, highestValue, lowestIndex)

## countVotes takes the highest voted candidate from each vote and counts all of
## them using the voteWeight of each vote.
##
## If more then one candiate are voted highest, all of them are counted with a reduced voteWeight.
##
## Candiates that are elected or eliminated are ignored.
countVotes : PollData -> List U64
countVotes = \pd ->
    ignore = ignoreIndex pd

    List.walkWithIndex
        pd.poll.votes
        (List.repeat 0 (List.len pd.poll.tieRank))
        \state, vote, index ->
            candidates = maxIndexes vote ignore
            if List.len candidates == 0 then
                state
            else
                weight =
                    when List.get pd.voteWeights index is
                        Ok v -> v // List.len candidates
                        Err OutOfBounds -> crash "vote has no weight. This should be checked in validate"

                List.walk candidates state \state2, candidateIndex ->
                    List.update state2 candidateIndex \v -> v + weight

expect
    pd = initialPollData examplePoll
    got = countVotes pd
    got == [0, 0, 0, 0, 0, 0, 0, 0, 10_000_000]

expect
    pd = initialPollData {
        seats: 3,
        votes: [
            [1, 1, 1],
        ],
        tieRank: [1, 2, 3],
    }
    got = countVotes pd
    got == [333_333, 333_333, 333_333]

updateVoteWeights : PollData, CandidateIndex, U64, U64 -> List U64
updateVoteWeights = \pd, candidateIndex, numberOfVotes, quota ->
    surplus = numberOfVotes - quota
    numOfPrefs = whoHasVoted pd candidateIndex

    List.map2
        numOfPrefs
        pd.voteWeights
        \numOfPref, voteWeight ->
            if numOfPref == 0 then
                voteWeight
            else
                x = (voteWeight // numOfPref)
                voteWeight - x + (x * surplus // numberOfVotes)

whoHasVoted : PollData, CandidateIndex -> List U64
whoHasVoted = \pd, winner ->
    ignore = ignoreIndex pd

    List.map
        pd.poll.votes
        \vote ->
            candidates = maxIndexes vote ignore
            if List.contains candidates winner then
                List.len candidates
            else
                0

examplePoll = {
    seats: 3,
    votes: [
        [1, 2, 3, 4, 5, 6, 7, 8, 9],
        [1, 2, 3, 4, 5, 6, 7, 8, 9],
        [1, 2, 3, 4, 5, 6, 7, 8, 9],
        [1, 2, 3, 4, 5, 6, 7, 8, 9],
        [1, 2, 3, 4, 5, 6, 7, 8, 9],
        [1, 2, 3, 4, 5, 6, 7, 8, 9],
        [1, 2, 3, 4, 5, 6, 7, 8, 9],
        [1, 2, 3, 4, 5, 6, 7, 8, 9],
        [1, 2, 3, 4, 5, 6, 7, 8, 9],
        [1, 2, 3, 4, 5, 6, 7, 8, 9],
    ],
    tieRank: [1, 2, 3, 4, 5, 6, 7, 8, 9],
}

resultMap2 : Result a err, Result b err, (a, b -> c) -> Result c err
resultMap2 = \r1, r2, fn ->
    # TODO: why does this not work?
    # when (r1, r2) is
    #     (Ok v1, Ok v2) -> Ok (fn v1 v2)
    #     (Err e1, _) -> r1
    #     (_, Err e2) -> r2
    when r1 is
        Ok v1 ->
            when r2 is
                Ok v2 ->
                    Ok (fn v1 v2)

                Err e2 -> Err e2

        Err e1 ->
            Err e1

expect
    got = resultMap2 (Ok 1) (Ok 2) \v, w -> v + w
    got == Ok 3

expect
    got = resultMap2 (Ok 1) (Err 2) \v, w -> v + w
    got == Err 2

## maxIndexes returns the indexes with the highest value.
## If more then one element have the same highest value, all of them are returned.
## 0 can never be the highest value. If all elements are 0, then a empty list is returned.
maxIndexes : List Preference, List CandidateIndex -> List Preference
maxIndexes = \list, ignore ->
    v = maxWithIgnore list ignore

    if v == 0 then
        []
    else
        findAllIndex list v

expect
    got = maxIndexes [1, 2, 3] []
    got == [2]

expect
    got = maxIndexes [3, 1, 3] []
    got == [0, 2]

expect
    got = maxIndexes [0, 0, 0] []
    got == []

expect
    got = maxIndexes [1, 2, 5] [2]
    got == [1]

expect
    got = maxIndexes [1, 2, 5] [2, 1, 0]
    got == []

## maxWighIgnore works like List.max but ignores elements at specific indexes.
##
## Returns 0 for an empty list of a list, that is fully ignored.
maxWithIgnore : List (Num a), List U64 -> Num a
maxWithIgnore = \list, ignore ->
    List.walkWithIndex
        list
        0
        \state, elem, index ->
            if List.contains ignore index then
                state
            else if elem > state then
                elem
            else
                state

findAllIndex : List a, a -> List U64 where a implements Eq
findAllIndex = \list, elem ->
    findAllIndexHelper list elem 0 []

findAllIndexHelper : List a, a, U64, List U64 -> List U64 where a implements Eq
findAllIndexHelper = \list, elem, index, result ->
    when list is
        [] -> result
        [first, .. as rest] ->
            newResult =
                if first == elem then
                    List.append result index
                else
                    result
            findAllIndexHelper rest elem (index + 1) newResult

expect
    got = findAllIndex [1, 2, 3] 3
    got == [2]

expect
    got = findAllIndex [1, 1, 3] 1
    got == [0, 1]

expect
    got = findAllIndex [1, 2, 3] 4
    got == []
