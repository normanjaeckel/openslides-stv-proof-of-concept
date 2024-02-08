port module Main exposing (main)

import Platform


main : Program Votes {} msg
main =
    Platform.worker { init = init, subscriptions = \_ -> Sub.none, update = \_ -> \_ -> ( {}, Cmd.none ) }


type alias Votes =
    { candidates : List String
    , seats : Int
    , votes : List (List Int)
    }


init : Votes -> ( {}, Cmd msg )
init votes =
    ( {}, getVotes <| calcVotes votes )


port getVotes : String -> Cmd msg



-- Algorithm


calcVotes : a -> String
calcVotes votes =
    "TODO"
