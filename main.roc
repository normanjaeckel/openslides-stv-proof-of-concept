app "single-transferable-vote"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br" }
    imports [pf.Stderr, pf.Stdout, pf.Task]
    provides [main] to pf

main =
    when singleTransferableVote examplePoll is
        Err e ->
            {} <- (Stderr.line "Error: $(e)") |> Task.await
            Task.err 1

        Ok winners ->
            winners
            |> List.map Num.toStr
            |> Str.joinWith ", "
            |> Stdout.line

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
    candidateWeight: [1, 2, 3, 4, 5, 6, 7, 8, 9],
}

# Single Transferable Vote Algorithm

Poll : {
    seats : U32,
    votes : List Vote,
    candidateWeight : List U32,
}

Vote : List Preference

Preference : U32

CandidateIndex : U32

singleTransferableVote : Poll -> Result (List CandidateIndex) Str
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

validate : Poll -> Result {} Str
validate = \poll ->
    Ok poll
    |> Result.try checkSeats
    |> Result.try checkCandidateWeight
    |> Result.try checkVotes
    |> Result.map \_ -> {}

checkSeats = \poll ->
    if poll.seats == 0 then
        Err "Number of seats must not be zero"
    else if Num.toU64 poll.seats > List.len poll.candidateWeight then
        Err "Number of seats must not be greater than number of candidates"
    else
        Ok poll

checkCandidateWeight = \poll ->
    if Set.fromList poll.candidateWeight |> Set.len != List.len poll.candidateWeight then
        Err "Values of candidate weight must be unique"
    else
        Ok poll

checkVotes = \poll ->
    numOfCandidates = List.len poll.candidateWeight
    if !(poll.votes |> List.all \vote -> List.len vote == numOfCandidates) then
        Err "Every vote must have exactly as many values as there candidates"
    else
        Ok poll

expect
    poll = { seats: 3, candidateWeight: [1, 2, 3], votes: [] }
    Result.isOk (validate poll)

expect
    poll = { seats: 0, candidateWeight: [1, 2, 3], votes: [] }
    validate poll == Err "Number of seats must not be zero"

expect
    poll = { seats: 4, candidateWeight: [1, 2, 3], votes: [] }
    validate poll == Err "Number of seats must not be greater than number of candidates"

expect
    poll = { seats: 2, candidateWeight: [1, 2, 2], votes: [] }
    validate poll == Err "Values of candidate weight must be unique"

expect
    poll = { seats: 2, candidateWeight: [1, 2, 3], votes: [[1, 2, 3], [1, 2]] }
    validate poll == Err "Every vote must have exactly as many values as there candidates"

expect
    poll = { seats: 2, candidateWeight: [1, 2, 3], votes: [[1, 2, 3], [1, 2, 3, 4]] }
    validate poll == Err "Every vote must have exactly as many values as there candidates"

preprocessVotes : Poll -> Poll
preprocessVotes = \poll ->
    { poll & votes: poll.votes |> List.map preprocessVote }

preprocessVote : Vote -> Vote
preprocessVote = \vote ->
    vote

expect
    got = preprocessVote [1, 1, 2, 3, 3, 3, 4]
    got == [1, 1, 3, 4, 4, 4, 7]

expect
    got = preprocessVote [20, 0, 10, 10, 0]
    got == [3, 0, 1, 1, 0]

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
        List.range { start: At 0, end: Length (List.len poll.candidateWeight) }
    voteWeights =
        List.repeat 1000000 (List.len poll.votes)

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
    # weightedFirstPreferences = {}
    sumOfWeightedFirstPreferences = {}
    quota = {}
    when getWinnerOrLooser pollData sumOfWeightedFirstPreferences quota is
        Winner w ->
            # surplus = {}
            voteWeights = pollData.voteWeights # TODO
            remainingSeats = pollData.remainingSeats - 1
            electedCandidates = pollData.electedCandidates |> List.append w
            hopefulCandidates = pollData.hopefulCandidates |> List.dropIf \c -> c == w
            remainingVotes = pollData.poll.votes # TODO
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
            remainingVotes = pollData.poll.votes # TODO
            { pollData &
                hopefulCandidates,
                eliminatedCandidates,
                remainingVotes,
            }

getWinnerOrLooser : PollData, {}, {} -> [Winner CandidateIndex, Looser CandidateIndex]
