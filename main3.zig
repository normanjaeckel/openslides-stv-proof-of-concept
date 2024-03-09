const std = @import("std");
const ArrayList = std.ArrayList;
const mem = std.mem;
const maxInt = std.math.maxInt;

const builtin = @import("builtin");
const global_allocator = if (builtin.target.cpu.arch == .wasm32) std.heap.wasm_allocator else std.heap.page_allocator;

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
    result[1] = @intCast(elected_candidates.len);
    @memcpy(result[2..], elected_candidates);
    // TODO: give JS the possibility to dealloc the result (also for the roc platform.)
    return result.ptr;
}

export fn allocUint32(length: u32) [*]u32 {
    const slice = global_allocator.alloc(u32, length) catch
        @panic("failed to allocate memory");

    return slice.ptr;
}

pub fn count(allocator: mem.Allocator, seats: u32, candidate_count: u32, vote_count: u32, data_pointer: [*]u32) ![]u32 {
    // TODO: validate poll
    const tie_rank = data_pointer[vote_count * candidate_count .. vote_count * candidate_count + candidate_count];
    const votes = try sortVotes(allocator, candidate_count, vote_count, data_pointer);
    defer {
        // TODO: the memory if votes seems a bit fragmented.
        for (votes) |vote| {
            for (vote) |group| {
                allocator.free(group);
            }
            allocator.free(vote);
        }
        allocator.free(votes);
    }

    var elected_candidates = try ArrayList(u32).initCapacity(allocator, seats);
    var ignore = ArrayList(u32).init(allocator);
    defer ignore.deinit();
    var vote_weights = try initWeights(allocator, @intCast(votes.len));
    defer allocator.free(vote_weights);
    var highest_candidates = try allocator.alloc(?[]u32, votes.len);
    defer allocator.free(highest_candidates);
    var counted_votes = try allocator.alloc(u64, candidate_count);
    defer allocator.free(counted_votes);

    while (true) {
        getHighest(&highest_candidates, votes, ignore.items);
        countVotes(&counted_votes, vote_weights, highest_candidates);
        const vote_sum = sum(counted_votes);

        if (vote_sum == 0) {
            break;
        }

        const remainining_seats = seats - elected_candidates.items.len;
        const quota: u32 = @intCast((vote_sum / (remainining_seats + 1)) + 1);
        const winner_looser = getWinnerAndLooser(counted_votes, tie_rank, ignore.items, quota);

        switch (winner_looser) {
            WinnerLooser.winner => |winner| {
                try elected_candidates.append(winner.candidate_idx);
                if (remainining_seats == 1) {
                    break;
                }
                try ignore.append(winner.candidate_idx);
                updateVoteWeights(&vote_weights, highest_candidates, winner, quota);
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
            for (vote_group) |candidate_idx| {
                result.*[candidate_idx] += weight;
            }
        }
    }
}

const Winner = struct {
    candidate_idx: u32,
    votes: u64,
};

const WinnerLooser = union(enum) {
    winner: Winner,
    looser: u32,
};

fn getWinnerAndLooser(counted: []u64, tie_rank: []u32, ignore: []u32, quota: u32) WinnerLooser {
    var lowest_value: u64 = maxInt(u64);
    var lowest_index: u32 = undefined;
    for (counted, 0..) |candidate_votes, i| {
        if (contains(ignore, @intCast(i))) {
            continue;
        }

        if (candidate_votes >= quota) {
            return WinnerLooser{ .winner = Winner{ .candidate_idx = @intCast(i), .votes = candidate_votes } };
        }

        if (candidate_votes < lowest_value or (candidate_votes == lowest_value and tie_rank[i] < tie_rank[lowest_index])) {
            lowest_value = candidate_votes;
            lowest_index = @intCast(i);
        }
    }
    return WinnerLooser{ .looser = lowest_index };
}

fn updateVoteWeights(vote_weights: *[]u32, highest_candidates: []?[]u32, winner: Winner, quota: u32) void {
    const surplus = winner.votes - quota;

    for (highest_candidates, 0..) |may_candidate_group, i| {
        if (may_candidate_group) |candidate_group| {
            if (contains(candidate_group, winner.candidate_idx)) {
                const x: u32 = vote_weights.*[i] / @as(u32, @intCast(candidate_group.len));
                vote_weights.*[i] = vote_weights.*[i] - x + @as(u32, @intCast(x * surplus / winner.votes));
            }
        }
    }
}

fn initWeights(allocator: mem.Allocator, vote_count: u32) ![]u32 {
    var vote_weights = try allocator.alloc(u32, vote_count);
    @memset(vote_weights, 1_000_000);
    return vote_weights;
}

fn sortVotes(allocator: mem.Allocator, candidate_count: u32, vote_count: u32, data_pointer: [*]u32) ![][][]u32 {
    var output = try allocator.alloc([][]u32, vote_count);
    var pref_index = try allocator.alloc(PrefIndex, candidate_count);
    defer allocator.free(pref_index);

    var i: usize = 0;
    while (i < vote_count) : (i += 1) {
        const from = i * candidate_count;
        const vote = data_pointer[from .. from + candidate_count];

        for (vote, 0..) |pref, candidate_idx| {
            pref_index[candidate_idx] = PrefIndex{ .amount = pref, .candidate = @intCast(candidate_idx) };
        }
        mem.sort(PrefIndex, pref_index, {}, cmpPrefIndex);
        output[i] = try unifyPrefIndex(allocator, pref_index);
    }

    return output;
}

const PrefIndex = struct { amount: u32, candidate: u32 };

fn cmpPrefIndex(_: void, a: PrefIndex, b: PrefIndex) bool {
    return a.amount > b.amount;
}

fn unifyPrefIndex(allocator: mem.Allocator, pref_index: []PrefIndex) ![][]u32 {
    var list = try ArrayList([]u32).initCapacity(allocator, pref_index.len);

    var i: usize = 0;
    while (i < pref_index.len) : (i += 1) {
        if (pref_index[i].amount == 0) {
            break;
        }

        if ((i == 0) or (pref_index[i - 1].amount != pref_index[i].amount)) {
            var l = try allocator.alloc(u32, 1);
            l[0] = pref_index[i].candidate;
            try list.append(l);
        } else {
            const old = list.pop();
            var new = try allocator.alloc(u32, old.len + 1);
            @memcpy(new[0..old.len], old);
            new[new.len - 1] = pref_index[i].candidate;
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
