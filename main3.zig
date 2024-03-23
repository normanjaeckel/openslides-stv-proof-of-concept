const std = @import("std");
const ArrayList = std.ArrayList;
const mem = std.mem;
const maxInt = std.math.maxInt;
const assert = std.debug.assert;

const builtin = @import("builtin");
const global_allocator = if (builtin.target.cpu.arch == .wasm32) std.heap.wasm_allocator else std.heap.page_allocator;

extern fn debug_string(str_bytes: ?[*]u8, str_len: usize) void;

fn debug(comptime fmt: []const u8, args: anytype) void {
    const line = std.fmt.allocPrint(global_allocator, fmt, args) catch unreachable;
    defer global_allocator.free(line);
    debug_string(line.ptr, line.len);
}

export fn single_transferable_vote(seats: u32, candidates: u32, votes: u32, data_pointer: [*]const u32) [*]u32 {
    const memory_size: usize = candidates * (votes + 1);
    defer global_allocator.free(data_pointer[0..memory_size]);

    const elected_candidates = count(global_allocator, seats, candidates, votes, data_pointer[0..memory_size]) catch unreachable;

    // TODO: give JS the possibility to dealloc the result (also for the roc platform.)
    return elected_candidates.ptr;
}

export fn allocUint32(length: u32) [*]u32 {
    const slice = global_allocator.alloc(u32, length) catch
        @panic("failed to allocate memory");

    return slice.ptr;
}

export fn deallocElectedCandidates(ptr: [*]u32) void {
    ElectedCandidateList.fromPointer(global_allocator, ptr).deinit();
}

const CandidateIdx = u32;

pub fn count(allocator: mem.Allocator, seats: u32, candidate_count: u32, vote_count: u32, raw_votes: []const u32) ![]u32 {
    // TODO: validate poll
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    const tie_rank = raw_votes[vote_count * candidate_count ..];
    var votes = try VoteList.init(arena_allocator, candidate_count, vote_count, raw_votes);

    const ignore_data = try arena_allocator.alloc(u32, candidate_count);
    var ignore = GrowList(u32).init(ignore_data);
    var vote_weights = try initWeights(arena_allocator, vote_count);
    var highest_candidates = try arena_allocator.alloc(?CandidateGroup, vote_count);
    var counted_votes = try arena_allocator.alloc(?u64, candidate_count);

    // Use the normal allocator here, so the results are not part of the arena
    var elected_candidates = try ElectedCandidateList.init(allocator, seats);

    while (true) {
        getHighest(&highest_candidates, &votes, ignore.items());
        const vote_sum = countVotes(&counted_votes, vote_weights, highest_candidates, ignore.items());

        if (vote_sum == 0) {
            break;
        }

        const remainining_seats = seats - elected_candidates.len;
        const quota: u32 = @intCast((vote_sum / (remainining_seats + 1)) + 1);

        const winner_looser = getWinnerOrLooser(counted_votes, tie_rank, quota);

        switch (winner_looser) {
            WinnerLooser.winner => |winner| {
                elected_candidates.add(winner.candidate_idx);
                if (remainining_seats == 1) {
                    break;
                }
                ignore.add(winner.candidate_idx);
                updateVoteWeights(&vote_weights, highest_candidates, winner, quota);
            },
            WinnerLooser.looser => |looser| {
                ignore.add(looser);
            },
        }
    }
    return elected_candidates.finalize();
}

fn getHighest(result: *[]?CandidateGroup, votes: *VoteList, ignore: []const CandidateIdx) void {
    for (votes.data, 0..) |*vote, i| {
        result.*[i] = for (vote.data[vote.start..]) |*group| {
            switch (group.remove_ignore(ignore)) {
                RemoveResult.IsNowEmpty => {
                    vote.start += 1;
                },
                RemoveResult.NotEmpty => {
                    break group.*;
                },
            }
        } else null;
    }
}

fn countVotes(result: *[]?u64, vote_weights: []const u32, vote_groups: []const ?CandidateGroup, ignore: []CandidateIdx) u64 {
    @memset(result.*, 0);
    for (ignore) |candidate_idx| {
        result.*[candidate_idx] = null;
    }

    var sum: u64 = 0;
    for (vote_groups, 0..) |may_vote_group, i| {
        if (may_vote_group) |vote_group| {
            const weight = vote_weights[i] / vote_group.len;
            for (vote_group.data) |candidate_idx| {
                if (result.*[candidate_idx]) |v| {
                    result.*[candidate_idx] = v + weight;
                }
            }
            sum += weight;
        }
    }
    return sum;
}

const Winner = struct {
    candidate_idx: u32,
    votes: u64,
};

const WinnerLooser = union(enum) {
    winner: Winner,
    looser: u32,
};

fn getWinnerOrLooser(counted: []const ?u64, tie_rank: []const CandidateIdx, quota: u32) WinnerLooser {
    var lowest_value: u64 = maxInt(u64);
    var lowest_index: u32 = undefined;
    for (counted, 0..) |may_candidate_votes, i| {
        if (may_candidate_votes) |candidate_votes| {
            const candidate_idx: CandidateIdx = @intCast(i);

            if (candidate_votes >= quota) {
                return WinnerLooser{ .winner = Winner{ .candidate_idx = candidate_idx, .votes = candidate_votes } };
            }

            if (candidate_votes < lowest_value or (candidate_votes == lowest_value and tie_rank[i] < tie_rank[lowest_index])) {
                lowest_value = candidate_votes;
                lowest_index = candidate_idx;
            }
        }
    }
    return WinnerLooser{ .looser = lowest_index };
}

fn updateVoteWeights(vote_weights: *[]u32, highest_candidates: []const ?CandidateGroup, winner: Winner, quota: u32) void {
    const surplus = winner.votes - quota;

    for (highest_candidates, 0..) |may_candidate_group, i| {
        if (may_candidate_group) |candidate_group| {
            if (contains(candidate_group.data, winner.candidate_idx)) {
                const x: u32 = vote_weights.*[i] / @as(u32, @intCast(candidate_group.len));
                vote_weights.*[i] = vote_weights.*[i] - x + @as(u32, @intCast(x * surplus / winner.votes));
            }
        }
    }
}

fn initWeights(allocator: mem.Allocator, vote_count: u32) ![]u32 {
    const vote_weights = try allocator.alloc(u32, vote_count);
    @memset(vote_weights, 1_000_000);
    return vote_weights;
}

const RemoveResult = enum {
    IsNowEmpty,
    NotEmpty,
};

const CandidateGroup = struct {
    data: []CandidateIdx,
    len: usize,

    fn remove_ignore(self: *CandidateGroup, ignore: []const CandidateIdx) RemoveResult {
        var idx: usize = self.len - 1;
        while (true) : (idx -= 1) {
            for (ignore) |ignore_element| {
                if (ignore_element == self.data[idx]) {
                    if (self.len == 1) {
                        return RemoveResult.IsNowEmpty;
                    }

                    self.data[idx] = self.data[self.len - 1];
                    self.len -= 1;
                    break;
                }
            }

            if (idx == 0) break;
        }
        return RemoveResult.NotEmpty;
    }

    test "remove an element" {
        var data = [_]u32{ 1, 2, 3 };
        var group = CandidateGroup{ .data = &data, .len = 3 };
        const got = group.remove_ignore(&[_]CandidateIdx{ 1, 3 });
        const new_data = group.data[0..group.len];
        try std.testing.expectEqualSlices(u32, &[_]CandidateIdx{2}, new_data);
        try std.testing.expectEqual(RemoveResult.NotEmpty, got);
    }
};

const Vote = struct {
    data: []CandidateGroup,
    start: usize,
};

const VoteList = struct {
    data: []Vote,

    fn init(allocator: mem.Allocator, candidate_count: u32, vote_count: u32, raw_votes: []const u32) !VoteList {
        var output = try allocator.alloc(Vote, vote_count);
        var pref_index = try allocator.alloc(PrefIndex, candidate_count);
        defer allocator.free(pref_index);

        var i: usize = 0;
        while (i < vote_count) : (i += 1) {
            const vote_idx = i * candidate_count;
            const vote = raw_votes[vote_idx .. vote_idx + candidate_count];

            for (vote, 0..) |pref, candidate_idx| {
                pref_index[candidate_idx] = PrefIndex{ .amount = pref, .candidate = @intCast(candidate_idx) };
            }
            mem.sort(PrefIndex, pref_index, {}, cmpPrefIndex);
            output[i] = try unifyPrefIndex(allocator, pref_index);
        }

        return VoteList{ .data = output };
    }

    fn deinit(self: VoteList, allocator: mem.Allocator) void {
        // TODO: the memory if votes seems a bit fragmented.
        for (self.data) |vote| {
            for (vote.data) |group| {
                allocator.free(group.data);
            }
            allocator.free(vote.data);
        }
        allocator.free(self.data);
    }

    const PrefIndex = struct { amount: u32, candidate: u32 };

    fn cmpPrefIndex(_: void, a: PrefIndex, b: PrefIndex) bool {
        return a.amount > b.amount;
    }

    fn unifyPrefIndex(allocator: mem.Allocator, pref_index: []const PrefIndex) !Vote {
        var list = try ArrayList(CandidateGroup).initCapacity(allocator, pref_index.len);

        var i: usize = 0;
        while (i < pref_index.len) {
            if (pref_index[i].amount == 0) {
                break;
            }

            var same_items: usize = 1;
            while (i + same_items < pref_index.len) : (same_items += 1) {
                if (pref_index[i].amount != pref_index[i + same_items].amount) {
                    break;
                }
            }

            var group = try allocator.alloc(u32, same_items);
            for (0..same_items) |j| {
                group[j] = pref_index[i + j].candidate;
            }

            try list.append(CandidateGroup{ .data = group, .len = same_items });
            i += same_items;
        }

        return Vote{ .data = try list.toOwnedSlice(), .start = 0 };
    }
};

// test "sort votes" {
//     const allocator = std.testing.allocator;

//     const got = try VoteList.init(allocator, 2, 3, &[_]u32{ 1, 2, 3, 3, 2, 1 });
//     defer got.deinit(allocator);

//     var parsed = try std.json.parseFromSlice(
//         [][][]u32,
//         allocator,
//         \\ [[[1],[0]], [[0,1]], [[0],[1]]]
//     ,
//         .{},
//     );
//     defer parsed.deinit();

//     try std.testing.expectEqualDeep(parsed.value, got.data);
// }

// test "sort votes with zero values" {
//     const allocator = std.testing.allocator;

//     const got = try VoteList.init(allocator, 2, 3, &[_]u32{ 0, 2, 0, 4, 0, 6 });
//     defer got.deinit(allocator);

//     var parsed = try std.json.parseFromSlice(
//         [][][]u32,
//         allocator,
//         \\[[[1]], [[1]], [[1]]]
//     ,
//         .{},
//     );
//     defer parsed.deinit();

//     try std.testing.expectEqualDeep(parsed.value, got.data);
// }

// test "same value" {
//     const allocator = std.testing.allocator;

//     const got = try VoteList.init(allocator, 2, 3, &[_]u32{ 2, 2, 2, 2, 2, 2 });
//     defer got.deinit(allocator);

//     var parsed = try std.json.parseFromSlice(
//         [][][]u32,
//         allocator,
//         \\[[[0,1]], [[0,1]], [[0,1]]]
//     ,
//         .{},
//     );
//     defer parsed.deinit();

//     try std.testing.expectEqualDeep(parsed.value, got.data);
// }

fn contains_list(a_list: []const u32, b_list: []const u32) bool {
    for (a_list) |a| {
        for (b_list) |b| {
            if (a == b) {
                return true;
            }
        }
    }
    return false;
}

fn contains(a_list: []const u32, v: u32) bool {
    for (a_list) |a| {
        if (a == v) {
            return true;
        }
    }
    return false;
}

fn GrowList(comptime T: type) type {
    return struct {
        const Self = @This();
        slice: []T,
        len: usize,

        fn init(slice: []T) Self {
            return Self{
                .slice = slice,
                .len = 0,
            };
        }

        fn add(self: *Self, v: T) void {
            assert(self.len <= self.slice.len);
            self.slice[self.len] = v;
            self.len += 1;
        }

        fn items(self: Self) []T {
            return self.slice[0..self.len];
        }
    };
}

const ElectedCandidateList = struct {
    allocator: mem.Allocator,
    slice: []CandidateIdx,
    len: usize,

    fn init(allocator: mem.Allocator, seats: u32) !ElectedCandidateList {
        const slice = try allocator.alloc(CandidateIdx, seats + 2);
        slice[0] = 0;
        slice[1] = seats;
        return ElectedCandidateList{
            .allocator = allocator,
            .slice = slice,
            .len = 0,
        };
    }

    fn fromPointer(allocator: mem.Allocator, ptr: [*]u32) ElectedCandidateList {
        // TODO: Handle error case
        assert(ptr[0] == 0);
        const len = ptr[1];
        return ElectedCandidateList{
            .allocator = allocator,
            .slice = ptr[0 .. len + 2],
            .len = len,
        };
    }

    fn deinit(self: ElectedCandidateList) void {
        self.allocator.free(self.slice);
    }

    fn finalize(self: ElectedCandidateList) []CandidateIdx {
        const v = self.allocator.resize(self.slice, self.len + 2);
        assert(v);
        self.slice[1] = self.len;
        return self.slice[0 .. self.len + 2];
    }

    fn add(self: *ElectedCandidateList, candidate_idx: CandidateIdx) void {
        assert(2 + self.len < self.slice.len);

        self.slice[2 + self.len] = candidate_idx;
        self.len += 1;
    }
};

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

    const raw_data = try data.rawVotes(allocator);
    defer allocator.free(raw_data);

    const result = try count(allocator, data.seats, data.candidateCount(), data.voteCount(), raw_data);
    defer allocator.free(result);
    try std.testing.expectEqualSlices(u32, data.expect, result[2..]);
}

const TestData = struct {
    seats: u32,
    tie_rank: []u32,
    votes: [][]u32,
    expect: []u32,

    pub fn rawVotes(self: TestData, allocator: std.mem.Allocator) ![]u32 {
        var result = try allocator.alloc(u32, (self.candidateCount() * (self.voteCount() + 1)));
        for (self.votes, 0..) |vote, i| {
            for (vote, 0..) |pref, j| {
                result[i * self.candidateCount() + j] = pref;
            }
        }
        for (self.tie_rank, 0..) |pref, i| {
            result[self.voteCount() * self.candidateCount() + i] = pref;
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
