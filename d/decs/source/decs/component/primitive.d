/// Defines wrapper around single field value components
module decs.component.primitive;

import decs.storage.vector;
import std.typecons;

/// Primitive component that wraps some primitive POD type.
struct PrimComponent(T, string nameImpl, StorageImpl = VecStorage!T) {
  T value;
  alias value this;
  alias Storage = StorageImpl;
  enum name = nameImpl;
}
