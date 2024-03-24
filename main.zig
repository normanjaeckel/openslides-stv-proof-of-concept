const std = @import("std");
const ArrayList = std.ArrayList;
const mem = std.mem;
const maxInt = std.math.maxInt;
const assert = std.debug.assert;

const stv = @import("./zig/v1.zig");

const builtin = @import("builtin");
const global_allocator = if (builtin.target.cpu.arch == .wasm32) std.heap.wasm_allocator else std.heap.page_allocator;

extern fn debug_string(str_bytes: ?[*]u8, str_len: usize) void;

fn debug(comptime fmt: []const u8, args: anytype) void {
    const line = std.fmt.allocPrint(global_allocator, fmt, args) catch unreachable;
    defer global_allocator.free(line);
    debug_string(line.ptr, line.len);
}

export fn single_transferable_vote(seats: u8, candidate_count: u8, vote_count: u32, data_pointer: [*]const u32) [*]u32 {
    const vote_data_size = candidate_count * vote_count;
    const memory_size: usize = vote_data_size + candidate_count;
    defer global_allocator.free(data_pointer[0..memory_size]);

    const elected_candidates = stv.count(
        global_allocator,
        seats,
        data_pointer[0..vote_data_size],
        data_pointer[vote_data_size..memory_size],
    ) catch unreachable;

    return elected_candidates.ptr;
}

export fn allocUint32(length: u32) [*]u32 {
    const slice = global_allocator.alloc(u32, length) catch
        @panic("failed to allocate memory");

    return slice.ptr;
}

export fn deallocElectedCandidates(ptr: [*]u32) void {
    std.debug.assert(ptr[0] == 0);
    const len = ptr[1];
    global_allocator.free(ptr[0..len]);
}

test "test 01 - only one winner because of many empty votes" {
    try callCount(
        \\{
        \\  "seats": 2,
        \\  "tie_rank": [1, 2, 3],
        \\  "votes": [[1, 0, 0], [0, 0, 0], [0, 0, 0]],
        \\  "expect": [0]
        \\}
    );
}

test "test 02" {
    try callCount(
        \\{
        \\  "seats": 1,
        \\  "tie_rank": [1, 2, 3],
        \\  "votes": [[1, 2, 3], [2, 3, 1], [3, 1, 2]],
        \\  "expect": [2]
        \\}
    );
}

test "test 03" {
    try callCount(
        \\{
        \\  "seats": 2,
        \\  "tie_rank": [1, 2, 3, 4],
        \\  "votes": [
        \\      [4, 1, 3, 2],
        \\      [2, 4, 1, 3],
        \\      [1, 4, 2, 3],
        \\      [1, 2, 4, 3],
        \\      [1, 4, 3, 0],
        \\      [3, 2, 4, 1],
        \\      [3, 4, 1, 2],
        \\      [3, 4, 1, 2],
        \\      [4, 3, 2, 0],
        \\      [2, 3, 4, 1]
        \\  ],
        \\  "expect": [1, 2]
        \\}
    );
}

test "test 04" {
    try callCount(
        \\{
        \\  "seats": 2,
        \\  "tie_rank": [1, 2, 3, 4],
        \\  "votes": [
        \\      [4, 1, 3, 2],
        \\      [2, 4, 1, 3],
        \\      [2, 4, 1, 3],
        \\      [1, 2, 4, 3],
        \\      [1, 4, 0, 3],
        \\      [3, 2, 4, 1],
        \\      [3, 4, 1, 2],
        \\      [3, 4, 1, 2],
        \\      [4, 3, 2, 0],
        \\      [2, 3, 4, 1]
        \\  ],
        \\  "expect": [1, 0]
        \\}
    );
}

test "test 05" {
    try callCount(
        \\{
        \\  "seats": 2,
        \\  "tie_rank": [1, 2, 3, 4],
        \\  "votes": [
        \\      [0, 0, 0, 0],
        \\      [0, 4, 0, 0],
        \\      [0, 4, 0, 0],
        \\      [0, 2, 0, 0],
        \\      [0, 4, 0, 0],
        \\      [0, 0, 0, 0],
        \\      [0, 4, 0, 0],
        \\      [0, 4, 0, 0],
        \\      [0, 0, 0, 0],
        \\      [0, 0, 0, 0]
        \\  ],
        \\  "expect": [1]
        \\}
    );
}

test "test 06" {
    try callCount(
        \\{
        \\  "seats": 2,
        \\  "tie_rank": [1, 2, 3, 4],
        \\  "votes": [
        \\      [0, 0, 0, 0],
        \\      [0, 4, 0, 0],
        \\      [0, 4, 0, 0],
        \\      [0, 2, 0, 0],
        \\      [0, 4, 0, 0],
        \\      [0, 0, 0, 0],
        \\      [1, 4, 0, 0],
        \\      [0, 4, 0, 1],
        \\      [1, 0, 0, 0],
        \\      [0, 0, 0, 1]
        \\  ],
        \\  "expect": [1, 3]
        \\}
    );
}

fn callCount(json_data: []const u8) !void {
    const allocator = std.testing.allocator;

    var parsed = try std.json.parseFromSlice(TestData, allocator, json_data, .{});
    defer parsed.deinit();

    const data = parsed.value;

    const raw_votes = try data.rawVotes(allocator);
    defer allocator.free(raw_votes);

    const result = try stv.count(allocator, data.seats, raw_votes, data.tie_rank);
    defer allocator.free(result);
    try std.testing.expectEqualSlices(u32, data.expect, result[2..]);
}

const TestData = struct {
    seats: u8,
    tie_rank: []u32,
    votes: [][]u32,
    expect: []u32,

    pub fn rawVotes(self: TestData, allocator: std.mem.Allocator) ![]u32 {
        var result = try allocator.alloc(u32, self.candidateCount() * (self.voteCount()));
        for (self.votes, 0..) |vote, i| {
            for (vote, 0..) |pref, j| {
                result[i * self.candidateCount() + j] = pref;
            }
        }
        return result;
    }

    pub fn candidateCount(self: TestData) u32 {
        return @intCast(self.tie_rank.len);
    }

    pub fn voteCount(self: TestData) u32 {
        return @intCast(self.votes.len);
    }
};
