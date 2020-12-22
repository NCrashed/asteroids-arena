/// Defines simple components for position, rotation, radius and etc
module asteroids.component.primitive;

import std.typecons;
import asteroids.v2;

/// Position component of each entity in world.
alias Position = Typedef!(v2f, v2f.init, "position");
/// Velocity component of each entity in world.
alias Velocity = Typedef!(v2f, v2f.init, "velocity");
/// Some entities has rotation component.
alias Rotation = Typedef!(float, float.init, "rotation");
/// Some entities has collision radius.
alias Radius = Typedef!(float, float.init, "radius");
/// Some entities has mass (but actually it is not used by any implementation at the moment)
alias Mass = Typedef!(float, float.init, "mass");
