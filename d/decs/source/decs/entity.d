module decs.entity;

/// Entity is simple ID that has associated components
alias Entity = size_t;

/// Special shortcut to context where there is only one meaningfull value
/// for entity ID.
immutable Entity global = 0;
