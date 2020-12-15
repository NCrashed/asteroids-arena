const std = @import("std");

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
    };
}
