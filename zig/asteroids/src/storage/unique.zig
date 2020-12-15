const std = @import("std");
const entity = @import("../entity.zig");

/// Memory storage for component of type 'T' that can have only one component
/// across all entities or none. 
pub fn UniqueStorage(comptime T: type) type {
    return struct {
        unique: ?T,
        owner: entity.Entity,

        pub const Self = UniqueStorage(T);
        pub const Element = T;

        /// Initialize the storage with 0 components
        pub fn init() Self {
            return Self {
                .unique = null,
                .owner = entity.global,
            };
        }

        /// Deallocate memory of the storage
        pub fn deinit(self: *Self) void {

        }
    };
}
