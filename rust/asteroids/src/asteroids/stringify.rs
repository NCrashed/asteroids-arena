use std::fmt::Display;

/// Helper to transform results from specific type of errors to string one without data
pub trait Stringify {
    fn stringify(self) -> Result<(), String>;
}

impl<T, E:Display> Stringify for Result<T, E> {
    fn stringify(self) -> Result<(), String> {
        self.map_err(|x| format!("{}", x)).map(|_| ())
    }
}
