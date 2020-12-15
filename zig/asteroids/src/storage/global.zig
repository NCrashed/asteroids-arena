const std = @import("std");

/// Memory storage for component of type 'T' that has always single
/// component across all world.
pub fn GlobalStorage(comptime T: type, comptime default: T) type {
    return struct {
        global: T,

        pub const Self = GlobalStorage(T, default);
        pub const Element = T;

        /// Initialize the storage with default value for global component
        pub fn init() Self {
            return Self {
                .global = default,
            };
        }

        /// Deallocate memory of the storage
        pub fn deinit(self: *Self) void {

        }
    };
}
