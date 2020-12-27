module decs.meta;

/// Mixin all required types that are required for library to work with user components.
///
/// $(B T) are all components that will be used in application.
mixin template DeclareComponents(T...) {
  mixin MakeStorages;
  mixin MakeEntitiesStorage!T;
}
