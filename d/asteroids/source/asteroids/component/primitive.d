/// Defines simple components for position, rotation, radius and etc
module asteroids.component.primitive;

public import asteroids.v2;
import decs.component.primitive;

/// Position component of each entity in world.
alias Position = PrimComponent!(v2f, "position");
/// Velocity component of each entity in world.
alias Velocity = PrimComponent!(v2f, "velocity");
/// Some entities has rotation component.
alias Rotation = PrimComponent!(float, "rotation");
/// Some entities has collision radius.
alias Radius = PrimComponent!(float, "radius");
/// Some entities has mass (but actually it is not used by any implementation at the moment)
alias Mass = PrimComponent!(float, "mass");
