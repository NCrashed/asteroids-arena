const std = @import("std");

/// Memory storage for component of type 'T' in growing array.
pub fn VecStorage(comptime T: type) type {
    return struct {
        components: std.ArrayList(T),

        /// Initialize the storage with 0 components
        pub fn init() VecStorage(T) {
            const allocator = std.heap.page_allocator;
            return VecStorage(T) {
                .components = std.ArrayList(T).init(allocator),
            };
        }

        /// Deallocate memory of the storage
        pub fn deinit(self: *VecStorage(T)) void {
            self.components.deinit();
        }
    };
}
