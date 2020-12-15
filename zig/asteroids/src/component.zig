/// Component tag that is used for fast calclutation whether
/// entity has required set of components.
pub const Component = enum(u16) {
    mass,
    player,
    position,
    radius,
    rotation,
    velocity
};
