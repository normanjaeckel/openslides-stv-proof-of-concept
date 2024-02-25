interface Poll
    exposes [
        Poll,
        Vote,
    ]
    imports []


Poll : {
    seats : U32,
    votes : List Vote,
    tieRank : List U32,
}

Vote : List U32
