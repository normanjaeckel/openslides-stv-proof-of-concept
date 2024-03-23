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

export fn single_transferable_vote(seats: u8, candidate_count: u8, vote_count: u32, data_pointer: [*]const u32) [*]u32 {
    const vote_data_size = candidate_count * vote_count;
    const memory_size: usize = vote_data_size + candidate_count;
    defer global_allocator.free(data_pointer[0..memory_size]);

    const elected_candidates = count(
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
    ElectedCandidateList.fromPointer(global_allocator, ptr).deinit();
}

const CandidateIdx = u32;
const VoteSum = u64;

pub fn count(allocator: mem.Allocator, seats: u8, raw_ballots: []const CandidateIdx, tie_rank: []const CandidateIdx) ![]CandidateIdx {
    // TODO: validate poll
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    const candidate_count = tie_rank.len;
    const vote_count = raw_ballots.len / candidate_count;

    var votes = try VoteList.init(arena_allocator, candidate_count, vote_count, raw_ballots);
    var ignore = GrowList(CandidateIdx).init(try arena_allocator.alloc(CandidateIdx, candidate_count));
    var vote_weights = try initWeights(arena_allocator, vote_count);
    var counted_votes = try arena_allocator.alloc(?VoteSum, candidate_count);

    // Use the normal allocator here, so the results are not part of the arena
    var elected_candidates = try ElectedCandidateList.init(allocator, seats);

    var highest_candidates = try getHighest(arena_allocator, votes);
    while (true) {
        @memset(counted_votes, 0);
        for (ignore.items()) |candidate_idx| {
            counted_votes[candidate_idx] = null;
        }
        const vote_sum = countVotes(&counted_votes, vote_weights, highest_candidates);

        if (vote_sum == 0) {
            break;
        }

        const remainining_seats = seats - elected_candidates.len;
        const quota = (vote_sum / (remainining_seats + 1)) + 1;

        const winner_looser = getWinnerOrLooser(counted_votes, tie_rank, quota);

        const remove_candidate = switch (winner_looser) {
            WinnerLooser.winner => |winner| blk: {
                elected_candidates.add(winner.candidate_idx);
                if (remainining_seats == 1) {
                    break;
                }
                updateVoteWeights(&vote_weights, highest_candidates, winner, quota);
                break :blk winner.candidate_idx;
            },
            WinnerLooser.looser => |looser| looser,
        };
        addIgnore(&highest_candidates, &votes, &ignore, remove_candidate);
    }
    return elected_candidates.finalize();
}

/// initWeights initializes the weight of each ballot.
fn initWeights(allocator: mem.Allocator, vote_count: usize) ![]VoteSum {
    const vote_weights = try allocator.alloc(VoteSum, vote_count);
    @memset(vote_weights, 1_000_000);
    return vote_weights;
}

/// getHighest returns the first preference from each ballot.
fn getHighest(allocator: mem.Allocator, votes: VoteList) ![]?CandidateGroup {
    var result = try allocator.alloc(?CandidateGroup, votes.data.len);
    for (votes.data, 0..) |*vote, i| {
        result[i] = if (vote.data.len > 0) vote.data[0] else null;
    }
    return result;
}

/// countVotes sums the vote_groups and writes the result into the first argument.
///
/// The sum is calculated, by adding the vote_weight of the user. If the user votes
/// for more then one candidate, the vote_weight is diveded equaly.
///
/// The function returns the sum of all votes.
fn countVotes(result: *[]?VoteSum, vote_weights: []const VoteSum, vote_groups: []const ?CandidateGroup) VoteSum {
    var sum: VoteSum = 0;
    for (vote_groups, 0..) |may_vote_group, i| {
        if (may_vote_group) |vote_group| {
            const weight = vote_weights[i] / vote_group.len;
            for (vote_group.data) |candidate_idx| {
                result.*[candidate_idx] = result.*[candidate_idx].? + weight;
            }
            sum += weight * vote_group.data.len;
        }
    }
    return sum;
}

const Winner = struct {
    candidate_idx: CandidateIdx,
    votes: VoteSum,
};

const WinnerLooser = union(enum) {
    winner: Winner,
    looser: CandidateIdx,
};

/// getWinnerOrLooser returns a Winner, if a candidate has quota or more votes,
/// or it returns the CandidateIdx with the lowest votes.
///
/// The tie_rank is used, if multiple users have the lowest votes. In this case, the
/// candidate with the lowest tie_rank is returned.
fn getWinnerOrLooser(counted: []const ?VoteSum, tie_rank: []const CandidateIdx, quota: VoteSum) WinnerLooser {
    var lowest_value: VoteSum = maxInt(VoteSum);
    var lowest_index: CandidateIdx = undefined;
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

/// updateVoteWeights reduces the vote_weight of each ballot, that voted for the winner.
fn updateVoteWeights(vote_weights: *[]VoteSum, highest_candidates: []const ?CandidateGroup, winner: Winner, quota: VoteSum) void {
    const surplus = winner.votes - quota;

    for (highest_candidates, 0..) |may_candidate_group, i| {
        if (may_candidate_group) |candidate_group| {
            if (contains(CandidateIdx, candidate_group.data, winner.candidate_idx)) {
                const x = vote_weights.*[i] / candidate_group.len;
                vote_weights.*[i] = vote_weights.*[i] - x + (x * surplus / winner.votes);
            }
        }
    }
}

/// addIgnore updated all memory, that depends on ignored candidates.
///
/// It removes it from highest_candidate and adds it to the list of ignored candidates.
fn addIgnore(highest_candidates: *[]?CandidateGroup, votes: *VoteList, ignore_list: *GrowList(CandidateIdx), new_ignore: CandidateIdx) void {
    ignore_list.add(new_ignore);

    for (0..highest_candidates.len) |i| {
        if (highest_candidates.*[i]) |*group| {
            if (group.remove_ignore(&[1]CandidateIdx{new_ignore}) == RemoveResult.IsNowEmpty) {
                // Take the next non-empty candidate_group out of the vote. Remove ignored candidates.
                var vote = votes.data[i];
                vote.start += 1;
                highest_candidates.*[i] = for (vote.data[vote.start..]) |*new_group| {
                    switch (new_group.remove_ignore(ignore_list.items())) {
                        RemoveResult.IsNowEmpty => {
                            vote.start += 1;
                        },
                        RemoveResult.NotEmpty => {
                            break new_group.*;
                        },
                    }
                } else null;
            }
        }
    }
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

    fn init(allocator: mem.Allocator, candidate_count: usize, vote_count: usize, raw_votes: []const CandidateIdx) !VoteList {
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

    const PrefIndex = struct { amount: CandidateIdx, candidate: CandidateIdx };

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

            var group = try allocator.alloc(CandidateIdx, same_items);
            for (0..same_items) |j| {
                group[j] = pref_index[i + j].candidate;
            }

            try list.append(CandidateGroup{ .data = group, .len = same_items });
            i += same_items;
        }

        return Vote{ .data = try list.toOwnedSlice(), .start = 0 };
    }
};

/// contains returns true, the list contains the element.
fn contains(comptime T: type, list: []const T, value: T) bool {
    for (list) |element| {
        if (element == value) {
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

    fn init(allocator: mem.Allocator, seats: u8) !ElectedCandidateList {
        const slice = try allocator.alloc(CandidateIdx, seats + 2);
        slice[0] = 0;
        slice[1] = seats;
        return ElectedCandidateList{
            .allocator = allocator,
            .slice = slice,
            .len = 0,
        };
    }

    fn fromPointer(allocator: mem.Allocator, ptr: [*]CandidateIdx) ElectedCandidateList {
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

    /// finalize frees all unneeded memory, except of the memory, that has to be returned by count()
    fn finalize(self: ElectedCandidateList) []CandidateIdx {
        const v = self.allocator.resize(self.slice, self.len + 2);
        assert(v);
        self.slice[1] = @intCast(self.len);
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

    const raw_votes = try data.rawVotes(allocator);
    defer allocator.free(raw_votes);

    const result = try count(allocator, data.seats, raw_votes, data.tie_rank);
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
