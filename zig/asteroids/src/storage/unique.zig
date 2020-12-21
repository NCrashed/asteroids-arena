const std = @import("std");
const entity = @import("../entity.zig");
const Entity = entity.Entity;

/// Memory storage for component of type 'T' that can have only one component
/// across all entities or none.
pub fn UniqueStorage(comptime T: type) type {
    return struct {
        unique: ?T,
        owner: Entity,

        pub const Self = UniqueStorage(T);
        pub const Element = T;

        /// Initialize the storage with 0 components
        pub fn init() Self {
            return Self{
                .unique = null,
                .owner = entity.global,
            };
        }

        /// Deallocate memory of the storage
        pub fn deinit(self: *Self) void {}

        /// Insert component inside the storage.
        pub fn insert(self: *Self, e: Entity, c: T) void {
            self.unique = c;
            self.owner = e;
        }
    };
}
