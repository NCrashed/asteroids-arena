module decs.storage.entity;

mixin template MakeEntitiesStorage(CS...) {
  import decs.entity;
  import decs.component.meta;
  import std.algorithm;
  import std.container.array;
  import std.range;

  alias AllComponents = AliasSeq!(Entities, CS);

  /// Pseudo component that has 'EntitiesStorage' storage type. The component is
  /// required for creation/deletion of entities and for changing
  struct Entities {
      /// Name of component. By the identificator you can get entities storage in
      /// mixined code.
      enum name = "entities";
      /// Storage for entities meta information.
      alias Storage = EntitiesStorage;
  }

  /// Special storage of world that tracks alive entities and their components.
  ///
  /// Template arguments are all supported entities.
  class EntitiesStorage {
    /// Next free entity ID
    size_t entityCounter = 0;
    /// Collection of alive entities
    Array!Entity alive;
    /// Components tags for iteration
    Array!ComponentTag tags;
    /// Entities that are marked for deletion
    Array!Entity deleted;

    /// Internal shorthand for all components
    alias CS = Components!AllComponents;

    /// Initialize storage
    this() {
      entityCounter = 0;
      alive = Array!Entity();
      tags = Array!ComponentTag();
      deleted = Array!Entity();
    }

    /// Create and registry new entity
    Entity create() {
      immutable e = entityCounter;
      entityCounter += 1;
      alive.insertBack(e);
      tags.insertBack(none);
      return e;
    }

    /// Remove entity from alive entities. Marks them as
    /// dead. They are deleted at end of frame with call
    /// to maintain method.
    void remove(Entity e) {
      deleted.insertBack(e);
    }

    /// Remove entity from alive entity. Pefrom destruction
    /// at the current moment, so alive entities change their
    /// ids. Don't use it when iterating over alive entities.
    void removeNow(Entity e) {
      foreach(i, ae; alive[].enumerate) {
        if (ae == e) {
          alive.swapRemove(i);
          tags.swapRemove(i);
          return;
        }
      }
    }

    /// Finalize removal of entities that was lazy deleted.
    /// After the operation indecies of alive entities are
    /// changed. The operation is designed to be called at
    /// end of frame.
    void maintain() {
      foreach(e; deleted[]) {
        removeNow(e);
      }
      deleted.clear();
    }

    /// Mark that entity has given components
    void addComponents(C...)(Entity e) {
      static assert(CS.hasAll!C, "Some components " ~ C.stringof
        ~ " is not known in world ones " ~ T.stringof);
      immutable i = aliveIndex(e);
      assert(i >= 0, "Entity " ~ e.stringof ~ " is dead!");
      tags[i] = tags[i] | CS.join!C;
    }

    /// Mark that entity doesn't have given components
    void removeComponents(C...)(Entity e) {
      static assert(CS.hasAll!C, "Some components " ~ C.stringof
        ~ " is not known in world ones " ~ T.stringof);
      immutable i = aliveIndex(e);
      assert(i >= 0, "Entity " ~ e.stringof ~ " is dead!");
      tags[i] = tags[i] & ~CS.join!C;
    }

    /// Return $(B true) if given alive entity (defined by index in alive array)
    /// has given components. Designed to be used acros iteration over all
    /// alive entities.
    bool aliveHas(C...)(size_t i) {
      static assert(CS.hasAll!C, "Some components " ~ C.stringof
        ~ " is not known in world ones " ~ T.stringof);
      return (tags[i] & CS.join!C) == CS.join!C;
    }

    /// Iteration over alive entities
    int opApply(scope int delegate(size_t, Entity) dg) {
      int result = 0;

      foreach (i, e; alive[].enumerate)
      {
          result = dg(i, e);
          if (result) break;
      }
      return result;
    }

    /// Get index in alive array for given entity. Returns -1 if it is not alive.
    size_t aliveIndex(Entity e) {
      foreach (i, ae; alive[].enumerate)
      {
          if (ae == e) return i;
      }
      return -1;
    }
  }

  /// Remove an element from array and swap last element to the hole
  private void swapRemove(T)(Array!T arr, size_t i) {
    arr[i] = arr[$-1];
    arr.removeBack();
  }
  unittest {
    auto a = Array!int(0, 2, 3, 4, 5);
    a.swapRemove(0);
    assert(a[].equal([5, 2, 3, 4]));
    a.swapRemove(3);
    assert(a[].equal([5, 2, 3]));
    a.swapRemove(1);
    assert(a[].equal([5, 3]));
    a.swapRemove(1);
    assert(a[].equal([5]));
    a.swapRemove(0);
    assert(a.length == 0);
  }
}
