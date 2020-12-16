const std = @import("std");
const Entity = @import("../entity.zig").Entity;

/// Memory storage for component of type 'T' in growing array.
pub fn VecStorage(comptime T: type) type {
    return struct {
        components: std.ArrayList(T),

        pub const Self = VecStorage(T);
        pub const Element = T;

        /// Initialize the storage with 0 components
        pub fn init() Self {
            const allocator = std.heap.page_allocator;
            return Self {
                .components = std.ArrayList(T).init(allocator),
            };
        }

        /// Deallocate memory of the storage
        pub fn deinit(self: *Self) void {
            self.components.deinit();
        }

        /// Insert component inside the storage.
        pub fn insert(self: *Self, e: Entity, c: T) !void {
            if (self.components.items.len <= e) {
                try self.components.resize(e+1);
            }
            self.components.items[e] = c;
        }

        /// Getting component from storage
        pub fn get(self: *const Self, e: Entity) ?T {
            if (e >= self.components.items.len) {
                return null;
            } else {
                return self.components.items[e];
            }
        }
    };
}
