const std = @import("std");

/// Memory storage for component of type 'T' that has always single
/// component across all world.
pub fn GlobalStorage(comptime T: type, comptime deinit: fn (*T) void) type {
    return struct {
        global: T,

        pub const Self = GlobalStorage(T, deinit);
        pub const Element = T;

        /// Initialize the storage with default value for global component
        pub fn init(value: T) Self {
            return Self{
                .global = value,
            };
        }

        /// Deallocate memory of the storage
        pub fn deinit(self: *Self) void {
            deinit(&self.global);
        }

        /// Insert component inside the storage.
        pub fn insert(self: *Self, e: Entity, c: T) void {
            self.global = c;
        }
    };
}
