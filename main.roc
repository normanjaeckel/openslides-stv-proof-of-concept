app "single-transferable-vote"
    packages { pf: "./platform/main.roc" }
    imports [pf.Poll.{ Poll, PollError, Preference, Vote }]
    provides [main] to pf

main : Poll -> Result (List U32) PollError
main = \poll ->
    singleTransferableVote poll

# Single Transferable Vote Algorithm

CandidateIndex : U32

singleTransferableVote : Poll -> Result (List CandidateIndex) PollError
singleTransferableVote = \poll ->
    poll
    |> validate
    |> Result.try
        \_ ->
            poll
            |> preprocessVotes
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
    else if Num.toU64 poll.seats > List.len poll.tieRank then
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

expect
    poll = { seats: 3, tieRank: [1, 2, 3], votes: [] }
    Result.isOk (validate poll)

expect
    poll = { seats: 0, tieRank: [1, 2, 3], votes: [] }
    validate poll == Err ZeroSeats

expect
    poll = { seats: 4, tieRank: [1, 2, 3], votes: [] }
    validate poll == Err MoreSeatsThanCandidates

expect
    poll = { seats: 2, tieRank: [1, 2, 2], votes: [] }
    validate poll == Err IdenticalTieRanks

expect
    poll = { seats: 2, tieRank: [1, 2, 3], votes: [[1, 2, 3], [1, 2]] }
    validate poll == Err InvalidVoteLength

expect
    poll = { seats: 2, tieRank: [1, 2, 3], votes: [[1, 2, 3], [1, 2, 3, 4]] }
    validate poll == Err InvalidVoteLength

preprocessVotes : Poll -> Poll
preprocessVotes = \poll ->
    { poll & votes: poll.votes |> List.map preprocessVote }

preprocessVote : Vote -> Vote
preprocessVote = \vote ->
    voteMapping =
        vote
        |> List.dropIf \pref -> pref == 0
        |> List.walk
            (Dict.empty {})
            \state, pref ->
                state
                |> Dict.update
                    pref
                    \possibleValue ->
                        when possibleValue is
                            Missing -> Present 1
                            Present n -> Present (n + 1)
        |> Dict.toList
        |> List.sortWith
            \(key1, _), (key2, _) -> Num.compare key1 key2
        |> List.walk
            ([], 1)
            \(mapping, nextNumber), (current, count) ->
                (mapping |> List.append (current, nextNumber), nextNumber + count)
        |> \(mapping, _) -> mapping
        |> Dict.fromList

    vote
    |> List.map
        \pref ->
            voteMapping |> Dict.get pref |> Result.withDefault 0

expect
    got = preprocessVote [1, 1, 2, 3, 3, 3, 4]
    got == [1, 1, 3, 4, 4, 4, 7]

expect
    got = preprocessVote [20, 0, 10, 10, 0]
    got == [3, 0, 1, 1, 0]

expect
    got = preprocessVote [20, 0, 10, 10, 20, 0, 0, 10, 11, 100]
    got == [5, 0, 1, 1, 5, 0, 0, 1, 4, 7]

PollData : {
    poll : Poll,
    hopefulCandidates : List CandidateIndex,
    electedCandidates : List CandidateIndex,
    eliminatedCandidates : List CandidateIndex,
    remainingSeats : U32,
    remainingVotes : List Vote,
    voteWeights : List U32,
}

toInitialPollData : Poll -> PollData
toInitialPollData = \poll ->
    hopefulCandidates =
        List.range { start: At 0, end: Length (List.len poll.tieRank) }
    voteWeights =
        List.repeat 1_000_000 (List.len poll.votes)

    {
        poll: poll,
        hopefulCandidates: hopefulCandidates,
        electedCandidates: [],
        eliminatedCandidates: [],
        remainingSeats: poll.seats,
        remainingVotes: poll.votes,
        voteWeights: voteWeights,
    }

expect
    got = toInitialPollData examplePoll
    got
    == {
        poll: examplePoll,
        hopefulCandidates: [0, 1, 2, 3, 4, 5, 6, 7, 8],
        electedCandidates: [],
        eliminatedCandidates: [],
        remainingSeats: 3,
        remainingVotes: examplePoll.votes,
        voteWeights: List.repeat 1000000 10,
    }

runSTVAlgorithmHelper : PollData -> List CandidateIndex
runSTVAlgorithmHelper = \pollData ->
    if pollData.remainingSeats == 0 then
        pollData.electedCandidates
    else
        runSTVAlgorithmHelper (runOneRound pollData)

runOneRound : PollData -> PollData
runOneRound = \pollData ->
    weightedFirstPreferences = getWeightedFirstPreferences
        pollData.remainingVotes
        pollData.voteWeights
    sumOfWeightedFirstPreferences = getSumOfWeightedFirstPreferences
        weightedFirstPreferences
        (List.len pollData.poll.tieRank)
    quota = (List.sum sumOfWeightedFirstPreferences // (pollData.remainingSeats + 1)) + 1

    when getWinnerOrLooser pollData sumOfWeightedFirstPreferences quota is
        Winner w ->
            # surplus = {}
            voteWeights = pollData.voteWeights # TODO use surplus
            remainingSeats = pollData.remainingSeats - 1
            electedCandidates = pollData.electedCandidates |> List.append w
            hopefulCandidates = pollData.hopefulCandidates |> List.dropIf \c -> c == w
            remainingVotes = pollData.poll.votes # TODO Besenkehrer
            { pollData &
                hopefulCandidates,
                electedCandidates,
                remainingSeats,
                remainingVotes,
                voteWeights,
            }

        Looser l ->
            eliminatedCandidates = pollData.eliminatedCandidates |> List.append l
            hopefulCandidates = pollData.hopefulCandidates |> List.dropIf \c -> c == l
            remainingVotes = pollData.poll.votes # TODO Besenkehrer
            { pollData &
                hopefulCandidates,
                eliminatedCandidates,
                remainingVotes,
            }

getWeightedFirstPreferences : List Vote, List U32 -> List (List U32)
getWeightedFirstPreferences = \votes, weights ->
    List.map2
        votes
        weights
        \vote, weight ->
            numOfFirstPrefs = vote |> List.countIf (\rank -> rank == 1) |> Num.toU32
            vote
            |> List.map \rank ->
                if rank == 1 then
                    (weight // numOfFirstPrefs) |> Num.toU32
                else
                    0

expect
    got = getWeightedFirstPreferences [[1, 2, 3], [2, 1, 3], [3, 1, 1], [0, 0, 0], [1, 1, 1]] [10, 20, 30, 40, 50]
    got == [[10, 0, 0], [0, 20, 0], [0, 15, 15], [0, 0, 0], [16, 16, 16]]

getSumOfWeightedFirstPreferences : List (List U32), U64 -> List U32
getSumOfWeightedFirstPreferences = \prefs, numOfCandidates ->
    sumList = List.repeat 0 numOfCandidates
    prefs
    |> List.walk
        sumList
        \state, pref ->
            List.map2 state pref \s, p -> s + p

expect
    got = getSumOfWeightedFirstPreferences [[10, 0, 0, 0], [0, 20, 0, 0], [0, 15, 15, 0], [0, 0, 0, 0], [16, 16, 16, 0]] 4
    got == [26, 51, 31, 0]

getWinnerOrLooser : PollData, List U32, U32 -> [Winner CandidateIndex, Looser CandidateIndex]

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
