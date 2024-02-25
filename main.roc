app "single-transferable-vote"
    packages { pf: "./platform/main.roc" }
    imports [pf.Poll.{ Poll, Vote }]
    provides [main] to pf

main : Poll -> List U32
main = \poll ->
    List.range { start: At 0, end: Before poll.seats }
