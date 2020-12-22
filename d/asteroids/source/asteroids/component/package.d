module asteroids.component;

import std.algorithm.iteration;
import std.meta;
import std.traits;
import std.typecons;

public import asteroids.component.player;
public import asteroids.component.primitive;
public import asteroids.component.size;

/// Component tag that is used for fast calclutation whether
/// entity has required set of components. Each component has
/// unique bit in the tag, so we can use bit masking for fast
/// checking whether entity has given set of components.
alias ComponentTag = uint;

/// Special tag that indicates that entity doesn't have any components
immutable ComponentTag none = 0;

/// Defines operations with components packed together.
template Components(T...) {
  /// Get unique tag value for the given type
  template tag(U) {
    enum i = staticIndexOf!(U, T);
    static if (i < 0) enum tag = 0;
    else enum tag = cast(ComponentTag)(1 << i);
  }

  /// Join tags for given components. Sampling components with that tag will
  /// return entities with all components enumerated as arguments.
  template join(U...) {
    private ComponentTag foldTags(ComponentTag[] xs) {
      return xs.fold!((a, b) => a | b)(0);
    }
    enum join = foldTags([staticMap!(tag, U)]);
  }

  /// Inserts storages for all components into scope. All components must have
  // defined name and Storage alias.
  mixin template Storages() {
    private mixin template Inner(U...) {
      static if (U.length == 0) {}
      else {
        import std.traits;
        mixin(fullyQualifiedName!(U[0].Storage) ~ " " ~ U[0].name ~ ";");
        mixin Inner!(U[1 .. $]);
      }
    }
    mixin Inner!(T);
  }
}

unittest {
  alias CS = Components!(Position, Rotation, Velocity, Radius);
  static assert(CS.tag!Position == 1);
  static assert(CS.tag!Rotation == 2);
  static assert(CS.tag!Velocity == 4);
  static assert(CS.tag!Radius == 8);
  static assert(CS.tag!Mass == 0);
}

unittest {
  alias CS = Components!(Position, Rotation, Velocity, Radius);
  static assert(CS.join!(Position) == 1);
  static assert(CS.join!(Position, Rotation) == 0b11);
  static assert(CS.join!(Position, Velocity) == 0b101);
  static assert(CS.join!(Position, Radius) == 0b1001);
  static assert(CS.join!(Radius) == 0b1000);
  static assert(CS.join!() == 0);
}

unittest {
  alias CS = Components!(Position, Rotation, Velocity, Radius);
  mixin CS.Storages;

  static assert(__traits(compiles, position));
  static assert(__traits(isSame, typeof(rotation), Rotation.Storage));
  static assert(!__traits(compiles, mass));
}