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

    const elected_candidates = count(global_allocator, seats, candidates, votes, data_pointer) catch undefined;
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

fn count(allocator: mem.Allocator, seats: u32, candidateCount: u32, voteCount: u32, data_pointer: [*]u32) ![]u32 {
    // TODO: validate poll
    const tieRank = data_pointer[voteCount * candidateCount .. voteCount * candidateCount + candidateCount];
    const votes = try sortVotes(allocator, candidateCount, voteCount, data_pointer);
    // TODO votes is [][][]u32, this call to free only frees the topmost slice.
    defer allocator.free(votes);
    var elected_candidates = try ArrayList(u32).initCapacity(allocator, seats);
    var ignore = ArrayList(u32).init(allocator);
    defer ignore.deinit();
    var vote_weights = try initWeights(allocator, votes.len);
    defer allocator.free(vote_weights);
    var highest_candidates = try allocator.alloc(?[]u32, votes.len);
    defer allocator.free(highest_candidates);
    var counted_votes = try allocator.alloc(u64, candidateCount);
    defer allocator.free(counted_votes);

    while (true) {
        getHighest(&highest_candidates, votes, ignore.items);
        countVotes(&counted_votes, vote_weights, highest_candidates);
        const vote_sum = sum(counted_votes);

        if (vote_sum == 0) {
            break;
        }

        const remaininingSeats = seats - elected_candidates.items.len;
        const quota: u32 = @intCast((vote_sum / (remaininingSeats + 1)) + 1);
        const winnerLooser = getWinnerAndLooser(counted_votes, tieRank, ignore.items, quota);

        switch (winnerLooser) {
            WinnerLooser.winner => |winner| {
                try elected_candidates.append(winner.candidateIdx);
                if (remaininingSeats == 1) {
                    break;
                }
                updateVoteWeights(&vote_weights, highest_candidates, winner, quota);
                try ignore.append(winner.candidateIdx);
            },
            WinnerLooser.looser => |looser| {
                try ignore.append(looser);
            },
        }
    }
    return try elected_candidates.toOwnedSlice();
}

fn getHighest(result: *[]?[]u32, votes: [][][]u32, ignore: []u32) void {
    for (votes, 0..) |vote, i| {
        result.*[i] = for (vote) |group| {
            if (!contains_list(group, ignore)) {
                break group;
            }
        } else null;
    }
}

fn countVotes(result: *[]u64, vote_weights: []u32, vote_groups: []?[]u32) void {
    @memset(result.*, 0);
    for (vote_groups, 0..) |may_vote_group, i| {
        if (may_vote_group) |vote_group| {
            const weight = vote_weights[i] / vote_group.len;
            for (vote_group) |candidateIdx| {
                result.*[candidateIdx] += weight;
            }
        }
    }
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

fn initWeights(allocator: mem.Allocator, voteCount: u32) ![]u32 {
    var vote_weights = try allocator.alloc(u32, voteCount);
    @memset(vote_weights, 1_000_000);
    return vote_weights;
}

fn sortVotes(allocator: mem.Allocator, candidate_count: u32, vote_count: u32, data_pointer: [*]u32) ![][][]u32 {
    var output = try allocator.alloc([][]u32, vote_count);

    var prefIndex = try allocator.alloc(PrefIndex, candidate_count);
    defer allocator.free(prefIndex);

    var i: usize = 0;
    while (i < vote_count) : (i += 1) {
        const from = i * candidate_count;
        const vote = data_pointer[from .. from + candidate_count];

        for (vote, 0..) |pref, candidateIdx| {
            // TODO: Ignore pref == 0
            //if (pref == 0) continue;
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

    var i: usize = 0;
    while (i < prefIndex.len) : (i += 1) {
        if ((i == 0) or (prefIndex[i - 1].amount != prefIndex[i].amount)) {
            var l = try allocator.alloc(u32, 1);
            l[0] = prefIndex[i].candidate;
            try list.append(l);
        } else {
            const old = list.pop();
            var new = try allocator.alloc(u32, old.len + 1);
            @memcpy(new, old);
            allocator.free(old);
            try list.append(new);
        }
    }

    return list.toOwnedSlice();
}

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
