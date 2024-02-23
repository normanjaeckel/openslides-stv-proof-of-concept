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
            |> List.map candidateToStr
            |> Str.joinWith ", "
            |> Stdout.line

examplePoll = {
    seats: 3,
    candidates: ["Ada", "Bert", "Claudine", "Deborah", "Emil", "Friederike", "Gregor", "Hassan", "Ida"],
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
}

candidateToStr = \cand ->
    cand

# Single Transferable Vote Algorithm

Poll : {
    seats : U64,
    candidates : List Candidate,
    votes : List Vote,
}

Candidate : Str

Vote : List Rank

Rank : U64

singleTransferableVote : Poll -> Result (List Candidate) Str
singleTransferableVote = \poll ->
    poll
    |> toPollData
    |> runSTVAlgorithmHelper
    |> Ok

PollData : {
    poll : Poll,
    hopefulCandidates : List CandidateIndex,
    electedCandidates : List CandidateIndex,
    rejectedCandidates : List CandidateIndex,
    remainingSeats : U64,
    remainingVotes : List Vote,
    voteWeights : List U64,
}

CandidateIndex : U64

toPollData : Poll -> PollData
toPollData = \poll ->
    hopefulCandidates =
        List.range { start: At 0, end: Length (List.len poll.candidates) }
    voteWeights =
        List.repeat 1000000 (List.len poll.votes)

    {
        poll: poll,
        hopefulCandidates: hopefulCandidates,
        electedCandidates: [],
        rejectedCandidates: [],
        remainingSeats: poll.seats,
        remainingVotes: poll.votes,
        voteWeights: voteWeights,
    }

expect
    got = toPollData examplePoll
    got
    == {
        poll: examplePoll,
        hopefulCandidates: [0, 1, 2, 3, 4, 5, 6, 7, 8],
        electedCandidates: [],
        rejectedCandidates: [],
        remainingSeats: 3,
        remainingVotes: examplePoll.votes,
        voteWeights: List.repeat 1000000 10,
    }

runSTVAlgorithmHelper : PollData -> List Candidate
runSTVAlgorithmHelper = \pollData ->
    if pollData.remainingSeats == 0 then
        pollData.electedCandidates
        |> List.map
            \idx ->
                when pollData.poll.candidates |> List.get idx is
                    Err OutOfBounds ->
                        crash "Impossible"

                    Ok cand ->
                        cand
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
            rejectedCandidates = pollData.rejectedCandidates |> List.append l
            hopefulCandidates = pollData.hopefulCandidates |> List.dropIf \c -> c == l
            remainingVotes = pollData.poll.votes # TODO
            { pollData &
                hopefulCandidates,
                rejectedCandidates,
                remainingVotes,
            }

getWinnerOrLooser : PollData, {}, {} -> [Winner CandidateIndex, Looser CandidateIndex]
