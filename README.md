# Proof of Concept for an implementation of the Single Transferable Vote electoral system

This is just a proof of concept. The algorithm is taken from [Raftery, et al., "The vote Package: Single Transferable Vote and Other Electoral Systems in R", The R Journal, 2021](https://doi.org/10.32614/RJ-2021-086), p. 681.

To build the zig implementation, call: zig build-lib -dynamic -target wasm32-freestanding main3.zig -femit-bin=main.wasm -rdynamic

Add `-O ReleaseSmall` to get an optimized build.
