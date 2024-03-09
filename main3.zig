const std = @import("std");
const global_allocator = std.heap.wasm_allocator;
const ArrayList = std.ArrayList;
const mem = std.mem;
const maxInt = std.math.maxInt;

extern fn debug_string(str_bytes: ?[*]u8, str_len: usize) void;

fn debug(comptime fmt: []const u8, args: anytype) void {
    const line = std.fmt.allocPrint(global_allocator, fmt, args) catch undefined;
    defer global_allocator.free(line);
    debug_string(line.ptr, line.len);
}

export fn single_transferable_vote(seats: u32, candidates: u32, votes: u32, data_pointer: [*]u32) [*]u32 {
    const memory_size: usize = candidates * (votes + 1);
    defer global_allocator.free(data_pointer[0..memory_size]);

    const poll = Poll.init(global_allocator, seats, candidates, votes, data_pointer) catch undefined;
    defer poll.deinit();

    const elected_candidates = poll.count() catch undefined;
    defer global_allocator.free(elected_candidates);

    var result = global_allocator.alloc(u32, elected_candidates.len + 2) catch undefined;
    result[0] = 0;
    result[1] = elected_candidates.len;
    @memcpy(result[2..], elected_candidates);
    // TODO: give JS the possibility to dealloc the result (also for the roc platform.)
    return result.ptr;
}

export fn allocUint32(length: u32) [*]u32 {
    const slice = global_allocator.alloc(u32, length) catch
        @panic("failed to allocate memory");

    return slice.ptr;
}

const Poll = struct {
    allocator: mem.Allocator,
    seats: u32,
    tieRank: []u32,
    votes: [][][]u32,

    pub fn init(allocator: mem.Allocator, seats: u32, candidateCount: u32, voteCount: u32, data_pointer: [*]u32) !Poll {
        // TODO: this is not realy necessary. It would probably be possible to have a function to get one vote out of the data_pointer.
        var vote_list = try allocator.alloc([]u32, voteCount);

        var vote_idx: usize = 0;
        while (vote_idx < voteCount) : (vote_idx += 1) {
            const from = vote_idx * candidateCount;
            vote_list[vote_idx] = data_pointer[from .. from + candidateCount];
        }

        // TODO: validate poll

        return Poll{
            .allocator = allocator,
            .seats = seats,
            .tieRank = data_pointer[voteCount * candidateCount .. voteCount * candidateCount + candidateCount],
            .votes = try sortVotes(allocator, candidateCount, vote_list),
        };
    }

    pub fn deinit(self: Poll) void {
        self.allocator.free(self.votes);
    }

    pub fn count(self: Poll) ![]u32 {
        var elected_candidates = try ArrayList(u32).initCapacity(self.allocator, self.seats);
        var ignore = ArrayList(u32).init(self.allocator);
        defer ignore.deinit();

        var vote_weights = try initWeights(self.allocator, self.votes.len);
        defer self.allocator.free(vote_weights);

        while (true) {
            // TODO dealocate or even better, allocate outsite the while loop.
            const highest_candidates = try getHighest(self.allocator, self.votes, ignore.items);
            const counted_votes = try countVotes(self.allocator, self.candidateLen(), vote_weights, highest_candidates);
            const vote_sum = sum(counted_votes);

            if (vote_sum == 0) {
                return try elected_candidates.toOwnedSlice();
            }

            const remaininingSeats = self.seats - elected_candidates.items.len;
            const quota: u32 = @intCast((vote_sum / (remaininingSeats + 1)) + 1);
            const winnerLooser = getWinnerAndLooser(counted_votes, self.tieRank, ignore.items, quota);

            switch (winnerLooser) {
                WinnerLooser.winner => |winner| {
                    try elected_candidates.append(winner.candidateIdx);
                    if (remaininingSeats == 1) {
                        return elected_candidates.toOwnedSlice();
                    }
                    updateVoteWeights(&vote_weights, highest_candidates, winner, quota);
                    try ignore.append(winner.candidateIdx);
                },
                WinnerLooser.looser => |looser| {
                    try ignore.append(looser);
                },
            }
        }
    }

    fn getHighest(allocator: mem.Allocator, votes: [][][]u32, ignore: []u32) ![]?[]u32 {
        var result = try allocator.alloc(?[]u32, votes.len);
        for (votes, 0..) |vote, i| {
            result[i] = for (vote) |group| {
                if (!contains_list(group, ignore)) {
                    break group;
                }
            } else null;
        }
        return result;
    }

    fn countVotes(allocator: mem.Allocator, candidateCount: u32, vote_weights: []u32, vote_groups: []?[]u32) ![]u64 {
        var result = try allocator.alloc(u64, candidateCount);
        @memset(result, 0);
        for (vote_groups, 0..) |may_vote_group, i| {
            if (may_vote_group) |vote_group| {
                const weight = vote_weights[i] / vote_group.len;
                for (vote_group) |candidateIdx| {
                    result[candidateIdx] += weight;
                }
            }
        }
        return result;
    }

    const Winner = struct {
        candidateIdx: u32,
        votes: u64,
    };

    const WinnerLooser = union(enum) {
        winner: Winner,
        looser: u32,
    };

    fn getWinnerAndLooser(counted: []u64, tie_rank: []u32, ignore: []u32, quota: u32) WinnerLooser {
        var lowest_value: u64 = maxInt(u64);
        var lowest_index: usize = undefined;
        for (counted, 0..) |candidate_votes, i| {
            if (contains(ignore, i)) {
                continue;
            }

            if (candidate_votes >= quota) {
                return WinnerLooser{ .winner = Winner{ .candidateIdx = i, .votes = candidate_votes } };
            }

            if (candidate_votes < lowest_value or (candidate_votes == lowest_value and tie_rank[i] < tie_rank[lowest_index])) {
                lowest_value = candidate_votes;
                lowest_index = i;
            }
        }
        return WinnerLooser{ .looser = lowest_index };
    }

    fn updateVoteWeights(vote_weights: *[]u32, highest_candidates: []?[]u32, winner: Winner, quota: u32) void {
        const surplus = winner.votes - quota;

        for (highest_candidates, 0..) |may_candidate_group, i| {
            if (may_candidate_group) |candidate_group| {
                if (contains(candidate_group, winner.candidateIdx)) {
                    const x = vote_weights.*[i] / candidate_group.len;
                    vote_weights.*[i] = vote_weights.*[i] - x + @as(u32, @intCast(x * surplus / winner.votes));
                }
            }
        }
    }

    fn candidateLen(self: Poll) u32 {
        return self.tieRank.len;
    }

    fn initWeights(allocator: mem.Allocator, voteCount: u32) ![]u32 {
        var vote_weights = try allocator.alloc(u32, voteCount);
        @memset(vote_weights, 1_000_000);
        return vote_weights;
    }

    fn sortVotes(allocator: mem.Allocator, candidateCount: u32, votes: [][]u32) ![][][]u32 {
        //TODO: ignore prefs==0, maybe in unifyPrefIndex
        var output = try allocator.alloc([][]u32, votes.len);
        var prefIndex = try allocator.alloc(PrefIndex, candidateCount);
        defer allocator.free(prefIndex);

        for (votes, 0..) |vote, i| {
            for (vote, 0..) |pref, candidateIdx| {
                prefIndex[candidateIdx] = PrefIndex{ .amount = pref, .candidate = candidateIdx };
            }
            mem.sort(PrefIndex, prefIndex, {}, cmpPrefIndex);

            output[i] = try unifyPrefIndex(allocator, prefIndex);
        }
        return output;
    }

    const PrefIndex = struct { amount: u32, candidate: usize };

    fn cmpPrefIndex(_: void, a: PrefIndex, b: PrefIndex) bool {
        return a.amount > b.amount;
    }

    fn unifyPrefIndex(allocator: mem.Allocator, prefIndex: []PrefIndex) ![][]u32 {
        var list = try ArrayList([]u32).initCapacity(allocator, prefIndex.len);
        defer list.deinit();

        var i: usize = 0;
        while (i < prefIndex.len) : (i += 1) {
            if ((i == 0) or (prefIndex[i - 1].amount != prefIndex[i].amount)) {
                var l = try allocator.alloc(u32, 1);
                l[0] = prefIndex[i].candidate;
                try list.append(l);
            } else {
                //TODO allocate new memory, free the old, copy the old data and
                //add the new candidate
            }
        }

        return list.toOwnedSlice();
    }
};

fn sum(slice: []u64) u64 {
    var result: u64 = 0;
    for (slice) |e| {
        result += e;
    }
    return result;
}

fn contains_list(a_list: []u32, b_list: []u32) bool {
    for (a_list) |a| {
        for (b_list) |b| {
            if (a == b) {
                return true;
            }
        }
    }
    return false;
}

fn contains(a_list: []u32, v: u32) bool {
    for (a_list) |a| {
        if (a == v) {
            return true;
        }
    }
    return false;
}
