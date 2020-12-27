module decs.component.meta;

import std.algorithm.iteration;
import std.meta;
import std.traits;
import std.typecons;

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
      return xs.fold!((a, b) => a | b)(cast(ComponentTag)0u);
    }
    enum join = foldTags([staticMap!(tag, U)]);
  }

  /// Generate code that inserts storages for components from $(B T) into scope. All components must have
  /// defined name and Storage alias.
  string storages(T...)() {
    string acc = "";
    static foreach(t; T) {
      acc ~= fullyQualifiedName!(t.Storage) ~ " " ~ t.name ~ ";\n";
    }
    return acc;
  }

  /// Return false if some components from $(B U) are not in $(B T)
  template hasAll(U...) {
    private template inComponents(C) {
      enum inComponents = staticIndexOf!(C, T) != -1;
    }
    enum hasAll = allSatisfy!(inComponents, U);
  }

  /// Make string for mixin expression that will collect Storages!U from local scope.
  string collect(U...)() {
    import std.range;

    string acc;
    static foreach(i, C; U) {
      acc ~= C.name ~ ", ";
    }
    return "Storages!"~U.stringof~"(" ~ acc.dropBack(2) ~ ")";
  }

  /// Make expressions that call init method for all storages
  string initStorages() {
    import std.meta;

    string acc;
    static foreach(i, C; T) {
      acc ~= C.name ~ "= new " ~ fullyQualifiedName!(C.Storage) ~ "();\n";
    }
    return acc;
  }
}

version(unittest) {
  import decs.component.primitive;
  alias Position = PrimComponent!(float[2], "position");
  alias Velocity = PrimComponent!(float[2], "velocity");
  alias Rotation = PrimComponent!(float, "rotation");
  alias Radius = PrimComponent!(float, "radius");
  alias Mass = PrimComponent!(float, "mass");
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

unittest {
  alias CS = Components!(Position, Rotation, Velocity, Radius);
  static assert(CS.hasAll!(Position));
  static assert(CS.hasAll!(Velocity, Radius));
  static assert(CS.hasAll!(Position, Rotation, Velocity, Radius));
  static assert(!CS.hasAll!(Mass));
  static assert(!CS.hasAll!(Rotation, Mass, Radius));
  static assert(!CS.hasAll!(Rotation, Mass));
}
