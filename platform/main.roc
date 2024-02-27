platform "stv-wasm"
    requires {} { main : Poll -> Result (List U32) PollError }
    exposes []
    packages {}
    imports [Poll.{ Poll, PollError }]
    provides [mainForHost]

mainForHost : Poll -> List U32
mainForHost = \poll ->
    when main poll is
        Ok winners ->
            List.prepend winners 0

        Err err ->
            when err is
                ZeroSeats -> [10]
                MoreSeatsThanCandidates -> [11]
                IdenticalTieRanks -> [12]
                InvalidVoteLength -> [13]

