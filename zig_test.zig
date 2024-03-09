const std = @import("std");
const testing = std.testing;

const stv = @import("main3.zig");

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

    const result = try stv.count(allocator, data.seats, data.candidateCount(), data.voteCount(), raw_data.ptr);
    defer allocator.free(result);
    try std.testing.expectEqualSlices(u32, result, data.expect);
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
