interface Poll
    exposes [
        Poll,
        Preference,
        Vote,
    ]
    imports []

Poll : {
    seats : U32,
    votes : List Vote,
    tieRank : List U32,
}

Vote : List Preference

Preference : U32
