platform "stv-wasm"
    requires {} { main : Poll -> List U32 }
    exposes []
    packages {}
    imports [Poll.{Poll}]
    provides [mainForHost]


mainForHost : Poll -> List U32
mainForHost = \poll ->
   main poll
    
