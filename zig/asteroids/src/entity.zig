const std = @import("std");
const Component = @import("component.zig").Component;
const builtin = @import("builtin");

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

    /// Mark that entity has given components
    pub fn add_component(self: *Entities, e: Entity, c: Component) !void {
        comptime var dochecks = builtin.mode == builtin.Mode.Debug;
        const mi = self.alive_index(e);
        if (dochecks) {
            if (mi == null) {
                return error.EntityDead;
            }
        }

        const oldtag = @enumToInt(self.tags.items[mi.?]);
        self.tags.items[mi.?] = @intToEnum(Component, oldtag & @enumToInt(c));
    }

    /// Mark that entity doesn't have given components
    pub fn remove_component(self: *Entities, e: Entity, c: Component) !void {
        comptime var dochecks = builtin.mode == builtin.Mode.Debug;
        if (dochecks and e >= self.alive.items.len) {
            return error.BoundsViolation;
        }
        const oldtag = @enumToInt(self.tags.items[e]);
        self.tags.items[e] = @intToEnum(Component, oldtag & !@enumToInt(c));
    }

    /// Return true if given entity has all requested components
    pub fn has(self: *const Entities, e: Entity, c: Component) !bool {
        comptime var dochecks = builtin.mode == builtin.Mode.Debug;
        if (dochecks and e >= self.alive.items.len) {
            return error.BoundsViolation;
        }
        const tag = @enumToInt(self.tags.items[e]);
        return tag & @enumToInt(c) == tag;
    }

    /// Function for iterating over all entities
    const EntityFun = fn (e: Entity) void;

    /// Iterate over all entities
    pub fn forall(self: *const Entities, comptime f: EntityFun) void {
        var i: usize = 0;
        while (i < self.alive.items.len) {
            f(self.alive.items[i]);
            i += 1;
        }
    }

    /// Iterate over all entities with given components set.
    pub fn join(self: *const Entities, c: Component, comptime f: EntityFun) void {
        var i: usize = 0;
        while (i < self.alive.items.len) {
            const tag = self.tags.items[i];
            if (tag & @enumToInt(c) == tag) {
                f(self.alive.items[i]);
            }
            i += 1;
        }
    }

    /// Find index in alive array for given entity
    pub fn alive_index(self: *const Entities, e: Entity) ?usize {
        var i: usize = 0;
        while (i < self.alive.items.len) {
            if (self.alive.items[i] == e) {
                return i;
            }
            i += 1;
        }
        return null;
    }
};
