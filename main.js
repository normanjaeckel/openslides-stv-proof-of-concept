async function load_wasm(wasm_file) {
  const importObj = {
    // TODO: Why do I have to define this?
    // Can probably be removed when this is fixed: https://github.com/roc-lang/roc/issues/5585
    wasi_snapshot_preview1: {
      proc_exit: (code) => {
        if (code !== 0) {
          console.error(`Exited with code ${code}`);
        }
      },
      fd_write: (x) => {
        console.error(`fd_write not supported: ${x}`);
      },
      random_get: (bufPtr, bufLen) => {
        const wasmMemoryBuffer = new Uint8Array(wasm.instance.exports.memory.buffer);
        const buf = wasmMemoryBuffer.subarray(bufPtr, bufPtr + bufLen);
        crypto.getRandomValues(buf);
        return 0;
      },
    },
    env: {
      roc_panic: (_pointer, _tag_id) => {
        throw "Roc panicked!";
      },
    },
  };

  const wasm = await WebAssembly.instantiateStreaming(fetch(wasm_file), importObj);
  const memory = wasm.instance.exports.memory;
  const single_transferable_vote = wasm.instance.exports.single_transferable_vote;
  const allocator = wasm.instance.exports.allocUint32;

  return function (seats, vote_list, tie_rank) {
    try {
      // TODO: Check that every vote and tie_rank has the same length.
      const candidates = vote_list[0].length;
      const votes = vote_list.length;
      const size = candidates * (votes + 1);
      const ptr = allocator(size);
      const slice = new Uint32Array(memory.buffer, ptr, size);
      for (let i = 0; i < votes; i++) {
        slice.set(vote_list[i], i * candidates);
      }
      slice.set(tie_rank, votes * candidates)

      // Call the roc code
      const result_pointer = single_transferable_vote(seats, candidates, votes, ptr);

      const result_slice = new Uint32Array(memory.buffer, result_pointer, seats + 1);
      if (result_slice[0] != 0) {
        throw "Error: Code " + result_slice[0]
      }
      let result = [];
      for (let i = 1; i <= seats; i++) {
        result.push(result_slice[i]);
      }
      return result;

    } catch (e) {
      const is_ok = e.message === "unreachable" && exit_code === 0;
      if (!is_ok) {
        console.error(e);
      }
    }
  }
}
