const std = @import("std");
const builtin = @import("builtin");
const glue = @import("glue");
const RocList = glue.list.RocList;
const allocator = std.heap.page_allocator;

comptime {
    if (builtin.target.cpu.arch != .wasm32) {
        @compileError("This platform is for WebAssembly only. You need to pass `--target wasm32` to the Roc compiler.");
    }
}

const Poll = extern struct {
    seats: u32,
    tieRank: RocList,
    votes: RocList,
};

extern fn roc__mainForHost_1_exposed_generic(*RocList, *Poll) void;
extern fn debug_string(str_bytes: ?[*]u8, str_len: usize) void;

fn debug(comptime fmt: []const u8, args: anytype) void {
    const line = std.fmt.allocPrint(allocator, fmt, args) catch undefined;
    defer allocator.free(line);
    debug_string(line.ptr, line.len);
}

// single_transferable_votes calculates the result for a list of votes.
//
// The first argument `seats` is the amount of positions, that should be voted.
// The second argument `candidates`is the amount of names, that are on each
// ballot. The third argument `votes` is the amount of votes. The forth arguent
// `data_pointer` is the memory-adress for the actual votes.
//
// The memory that `data_pointer` is a list of  `candidates` * (`votes` + 1) u32
// values. Each group of numbers (amount of `candidates`) is one ballot. The
// last group is like a ballot. It has to contain unique numbers and is used, if
// there is a tie.
//
// For example, if there are two candidates and trey votes, the memory could
// look like this: `(1,2),(2,1),(1,1),(1,2)` (The paranthesize are only for
// better visibility). In this example, the first vote prefers 1 over 2, the
// second 2 over 1, the thrid has no preference. If there is a tie, 1 wins over
// 2.
export fn single_transferable_vote(seats: u32, candidates: u32, votes: u32, data_pointer: [*]u32) ?[*]u8 {
    const memory_size: usize = candidates * (votes + 1);

    defer allocator.free(data_pointer[0..memory_size]);

    var vote_list = allocator.alloc(RocList, votes) catch unreachable;
    var vote_idx: usize = 0;
    var num: u32 = 0;
    while (vote_idx < votes) : (vote_idx += 1) {
        var vote = allocator.alloc(u32, candidates) catch unreachable;
        // TODO: errdefer allocator.dealloc(foo.data);

        var candidate_idx: usize = 0;
        while (candidate_idx < candidates) : (candidate_idx += 1) {
            vote[candidate_idx] = data_pointer[vote_idx * candidates + candidate_idx];
            num += data_pointer[vote_idx * candidates + candidate_idx];
        }
        vote_list[vote_idx] = RocList.fromSlice(u32, vote);
    }

    var tieRank = allocator.alloc(u32, candidates) catch unreachable;
    // TODO: errdefer allocator.dealloc(foo.data);
    var candidate_idx: usize = 0;
    while (candidate_idx < candidates) : (candidate_idx += 1) {
        tieRank[candidate_idx] = data_pointer[votes * candidates + candidate_idx];
        num += data_pointer[votes * candidates + candidate_idx];
    }

    // defer {
    //     roc_dealloc(raw_output, @alignOf(u64));
    // }

    var poll = Poll{
        .seats = seats,
        .votes = RocList.fromSlice(RocList, vote_list),
        .tieRank = RocList.fromSlice(u32, tieRank),
    };

    var result: RocList = undefined;
    allocations = .{ .memory = 0, .count = 0 };
    roc__mainForHost_1_exposed_generic(&result, &poll);
    debug("allocations: {} {}", .{ allocations.memory, allocations.count });
    return result.getAllocationPtr();
}

export fn allocUint32(length: u32) [*]u32 {
    const slice = allocator.alloc(u32, length) catch
        @panic("failed to allocate memory");

    return slice.ptr;
}

pub fn main() u8 {
    // TODO: This should be removed: https://github.com/roc-lang/roc/issues/5585
    return 0;
}

const Align = @alignOf(usize);
extern fn malloc(size: usize) callconv(.C) ?*align(Align) anyopaque;
extern fn realloc(c_ptr: [*]align(Align) u8, size: usize) callconv(.C) ?*anyopaque;
extern fn free(c_ptr: [*]align(Align) u8) callconv(.C) void;
extern fn memcpy(dst: [*]u8, src: [*]u8, size: usize) callconv(.C) void;
extern fn memset(dst: [*]u8, value: i32, size: usize) callconv(.C) void;

var allocations: struct { memory: usize, count: usize } = .{ .memory = 0, .count = 0 };

export fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    allocations.memory += size;
    allocations.count += 1;
    _ = alignment;
    return malloc(size);
}

export fn roc_realloc(c_ptr: *anyopaque, new_size: usize, old_size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    _ = old_size;
    _ = alignment;
    return realloc(@as([*]align(Align) u8, @alignCast(@ptrCast(c_ptr))), new_size);
}

export fn roc_dealloc(c_ptr: *anyopaque, alignment: u32) callconv(.C) void {
    _ = alignment;
    free(@as([*]align(Align) u8, @alignCast(@ptrCast(c_ptr))));
}
