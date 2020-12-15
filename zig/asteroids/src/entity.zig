const std = @import("std");
const Component = @import("component.zig").Component;

/// Entity is simple ID that has associated components
pub const Entity = u32;

/// Special shortcut to context where there is only one meaningfull value
/// for entity ID.
pub const global : Entity = 0;

/// Special storage of world that tracks alive entities and their components.
pub const Entities = struct {
    /// Next free entity ID
    entity_counter: u32,
    /// Collection of alive entities
    alive: std.ArrayList(Entity),
    /// Component tags for iteration
    tags: std.ArrayList(Component),

    /// Initialize the storage with 0 entities
    pub fn init() Entities {
        const allocator = std.heap.page_allocator;
        return Entities {
            .entity_counter = 0,
            .alive = std.ArrayList(Entity).init(allocator),
            .tags = std.ArrayList(Component).init(allocator),
        };
    }

    /// Deallocate memory of the storage
    pub fn deinit(self: *Entities) void {
        self.alive.deinit();
        self.tags.deinit();
    }

    /// Create and registry new entity
    pub fn new(self: *Entities) !Entity {
        const e = self.entity_counter;
        self.entity_counter += 1;
        try self.alive.append(e);
        try self.tags.append(Component.none());
        return e;
    }

    /// Remove entity from alive entities. Returns false if
    /// the entity has been dead already.
    pub fn destroy(self: *Entities, e: Entity) bool {
        var i: usize = 0;
        while (i < self.alive.items.len) {
            if (self.alive.items[i] == e) {
                self.alive.swapRemove(i);
                self.tags.swapRemove(i);
            }
            i += 1;
        }
        return false;
    }
};
