/// Component tag that is used for fast calclutation whether
/// entity has required set of components.
pub const Component = enum(u16) {
    position = 0x01,
    rotation = 0x02,
    velocity = 0x04,
    mass = 0x08,
    radius = 0x10,
    player = 0x20,
    asteroid = 0x40,
    bullet = 0x80,
    _,

    /// Return value that indicates that there are no components created
    pub fn none() Component {
        return @intToEnum(Component, 0);
    }

    /// Combine several component tags into single one
    pub fn combine(comptime args: anytype) Component {
        comptime var acc = 0;
        inline for (args) |c, i| {
            acc = acc | @enumToInt(args[i]);
        }
        return @intToEnum(Component, acc);
    }
};
