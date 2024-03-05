app "single-transferable-vote"
    packages {
        pf: "./platform/main.roc",
        tests: "./tests/main.roc",
    }
    imports [
        pf.Poll.{ CandidateIndex, NumOfSeats, Poll, PollError, Preference, Vote },
        tests.Suite,
    ]
    provides [main] to pf

main : Poll -> Result (List CandidateIndex) PollError
main = \poll ->
    poll
    |> validate
    |> Result.try
        \_ ->
            poll
            |> toInitialPollData
            |> runSTVAlgorithmHelper
            |> Ok

validate : Poll -> Result {} PollError
validate = \poll ->
    Ok poll
    |> Result.try checkSeats
    |> Result.try checkTieRank
    |> Result.try checkVotes
    |> Result.map \_ -> {}

checkSeats = \poll ->
    if poll.seats == 0 then
        Err ZeroSeats
    else
        numOfCandidates = List.len poll.tieRank
        if poll.seats > numOfCandidates then
            Err MoreSeatsThanCandidates
        else if poll.seats == numOfCandidates then
            Err EqualSeatsThanCandidates
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
    else if poll.votes |> List.all \vote -> (List.max vote |> Result.withDefault 0) == 0 then
        Err EmptyVotes
    else
        Ok poll

expect
    poll = { seats: 2, votes: [[1, 2, 3], [1, 2, 3], [1, 2, 3]], tieRank: [1, 2, 3] }
    Result.isOk (validate poll)

expect
    poll = { seats: 0, tieRank: [1, 2, 3], votes: [] }
    validate poll == Err ZeroSeats

expect
    poll = { seats: 4, tieRank: [1, 2, 3], votes: [] }
    validate poll == Err MoreSeatsThanCandidates

expect
    poll = { seats: 3, tieRank: [1, 2, 3], votes: [] }
    validate poll == Err EqualSeatsThanCandidates

expect
    poll = { seats: 2, tieRank: [1, 2, 2], votes: [] }
    validate poll == Err IdenticalTieRanks

expect
    poll = { seats: 2, tieRank: [1, 2, 3], votes: [[1, 2, 3], [1, 2]] }
    validate poll == Err InvalidVoteLength

expect
    poll = { seats: 2, tieRank: [1, 2, 3], votes: [[1, 2, 3], [1, 2, 3, 4]] }
    validate poll == Err InvalidVoteLength

expect
    poll = { seats: 2, tieRank: [1, 2, 3], votes: [] }
    validate poll == Err EmptyVotes

expect
    poll = { seats: 2, tieRank: [1, 2, 3], votes: [[0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]] }
    validate poll == Err EmptyVotes

PollData : {
    poll : Poll,
    numOfCandidates : NumOfCandidates,
    hopefulCandidates : List CandidateIndex,
    electedCandidates : List CandidateIndex,
    remainingSeats : NumOfSeats,
    voteWeights : List VoteWeight,
}

VoteWeight : U64

NumOfCandidates : U64

toInitialPollData : Poll -> PollData
toInitialPollData = \poll ->
    numOfCandidates = List.len poll.tieRank
    hopefulCandidates =
        List.range { start: At 0, end: Length numOfCandidates }
    voteWeights =
        List.repeat 1_000_000 (List.len poll.votes)
    {
        poll: poll,
        numOfCandidates: numOfCandidates,
        hopefulCandidates: hopefulCandidates,
        electedCandidates: [],
        remainingSeats: poll.seats,
        voteWeights: voteWeights,
    }

expect
    got = toInitialPollData examplePoll
    got
    == {
        poll: examplePoll,
        numOfCandidates: 9,
        hopefulCandidates: [0, 1, 2, 3, 4, 5, 6, 7, 8],
        electedCandidates: [],
        remainingSeats: 3,
        voteWeights: List.repeat 1_000_000 10,
    }

runSTVAlgorithmHelper : PollData -> List CandidateIndex
runSTVAlgorithmHelper = \pollData ->
    when runOneRound pollData is
        Done electedCandidates ->
            electedCandidates

        Continue newPollData ->
            runSTVAlgorithmHelper newPollData

runOneRound : PollData -> [Done (List CandidateIndex), Continue PollData]
runOneRound = \pollData ->
    (countedVotes, votesList) = countVotes
        pollData.hopefulCandidates
        pollData.poll.votes
        pollData.voteWeights
        pollData.numOfCandidates

    if List.max countedVotes |> Result.withDefault 0 == 0 then
        Done pollData.electedCandidates
    else
        quota = (List.sum countedVotes // (pollData.remainingSeats + 1)) + 1

        when getWinnerOrLooser pollData.hopefulCandidates countedVotes quota pollData.poll.tieRank is
            Winner winner winnerVoteValue ->
                electedCandidates = pollData.electedCandidates |> List.append winner
                remainingSeats = pollData.remainingSeats - 1
                if remainingSeats == 0 then
                    Done electedCandidates
                else
                    hopefulCandidates = pollData.hopefulCandidates |> List.dropIf \c -> c == winner

                    surplusNumerator = (winnerVoteValue - quota)
                    surplusDenominator = winnerVoteValue
                    voteWeights =
                        recomputeVoteWeights
                            pollData.voteWeights
                            votesList
                            winner
                            surplusNumerator
                            surplusDenominator

                    Continue
                        { pollData &
                            hopefulCandidates,
                            electedCandidates,
                            remainingSeats,
                            voteWeights,
                        }

            Looser looser ->
                hopefulCandidates = pollData.hopefulCandidates |> List.dropIf \c -> c == looser
                if List.len hopefulCandidates == pollData.remainingSeats then
                    Done (List.join [pollData.electedCandidates, hopefulCandidates])
                else
                    Continue { pollData & hopefulCandidates }

countVotes : List CandidateIndex, List Vote, List VoteWeight, NumOfCandidates -> (List VoteWeight, List (List CandidateIndex))
countVotes = \hopefulCandidates, votes, weights, numOfCandidates ->
    List.map2 votes weights (\v, w -> (v, w))
    |> List.walk
        { counted: List.repeat 0 numOfCandidates, votesList: [] }
        \state, (vote, weight) ->
            vote
            |> List.walkWithIndex
                { rank: 0, indexes: [] }
                \innerState, currentRank, currentIndex ->
                    if currentRank == 0 then
                        innerState
                    else if !(hopefulCandidates |> List.contains currentIndex) then
                        innerState
                    else if currentRank < innerState.rank then
                        innerState
                    else if currentRank > innerState.rank then
                        { rank: currentRank, indexes: [currentIndex] }
                    else
                        # currentRank == innerState.rank
                        { innerState & indexes: innerState.indexes |> List.append currentIndex }
            |> \{ indexes } ->
                newVotesList = state.votesList |> List.append indexes
                newCounted =
                    if List.isEmpty indexes then
                        state.counted
                    else
                        value = weight // List.len indexes
                        indexes
                        |> List.walk
                            state.counted
                            \innerState, idx ->
                                innerState |> List.update idx (\current -> current + value)
                { counted: newCounted, votesList: newVotesList }
    |> \s ->
        (s.counted, s.votesList)

expect
    got = countVotes [0, 2, 1, 0] [[3, 2, 1, 0], [2, 3, 1, 0], [1, 5, 5, 0], [0, 0, 0, 0], [1, 1, 1, 0]] [10, 20, 30, 40, 50] 4
    got == ([26, 51, 31, 0], [[0], [1], [1, 2], [], [0, 1, 2]])

getWinnerOrLooser : List CandidateIndex, List VoteWeight, U64, List U64 -> [Winner CandidateIndex VoteWeight, Looser CandidateIndex]
getWinnerOrLooser = \hopefulCandidates, countedVotes, quota, tieRank ->
    max = List.max countedVotes |> Result.withDefault 0
    if max >= quota then
        List.map2 countedVotes tieRank \a, b -> (a, b)
        |> List.walkWithIndex
            (Num.minU64, 0, 0)
            \(previousValue, previousTieRank, previousIndex), (thisValue, thisTieRank), thisIndex ->
                switch =
                    if previousValue > thisValue then
                        Previous
                    else if previousValue < thisValue then
                        This
                    else if previousTieRank < thisTieRank then
                        This
                    else
                        Previous

                when switch is
                    Previous ->
                        (previousValue, previousTieRank, previousIndex)

                    This ->
                        (thisValue, thisTieRank, thisIndex)
        |> \(_, _, idx) ->
            Winner idx max
    else
        List.map2 countedVotes tieRank \a, b -> (a, b)
        |> List.walkWithIndex
            (Num.maxU64, 0, 0)
            \(previousValue, previousTieRank, previousIndex), (thisValue, thisTieRank), thisIndex ->
                switch =
                    if !(hopefulCandidates |> List.contains thisIndex) then
                        Previous
                    else if previousValue < thisValue then
                        Previous
                    else if previousValue > thisValue then
                        This
                    else if previousTieRank > thisTieRank then
                        This
                    else
                        Previous

                when switch is
                    Previous ->
                        (previousValue, previousTieRank, previousIndex)

                    This ->
                        (thisValue, thisTieRank, thisIndex)
        |> \(_, _, idx) ->
            Looser idx

expect
    got = getWinnerOrLooser [0, 1, 2, 3, 4] [20, 10, 10, 20, 20] 21 [3, 2, 1, 5, 4]
    got == Looser 2

expect
    got = getWinnerOrLooser [0, 1, 2, 3, 4] [20, 10, 10, 20, 20] 20 [3, 2, 1, 5, 4]
    got == Winner 3 20

expect
    got = getWinnerOrLooser [0, 1, 2, 3, 4] [20, 10, 10, 20, 20] 10 [3, 2, 1, 5, 4]
    got == Winner 3 20

recomputeVoteWeights : List VoteWeight, List (List CandidateIndex), CandidateIndex, U64, U64 -> List VoteWeight
recomputeVoteWeights = \voteWeights, voteList, candidate, surplusNumerator, surplusDenominator ->
    List.map2
        voteWeights
        voteList
        \voteWeight, indexes ->
            if indexes |> List.contains candidate then
                valueForCandidate = voteWeight // List.len indexes
                voteWeight - valueForCandidate + (valueForCandidate * surplusNumerator // surplusDenominator)
            else
                voteWeight

expect
    got = recomputeVoteWeights [30, 20, 10] [[2], [0, 2], [3, 4]] 2 19 40
    got == [14, 14, 10]

# Test data

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

expect
    got = main examplePoll
    got == Ok [8, 7, 6]

expect
    Suite.suite
    |> List.map
        \(name, poll, expected) ->
            got = main poll
            expect name == name && got == expected
            0
    |> List.max
    |> Result.withDefault 0
    |> \v -> v == 0
