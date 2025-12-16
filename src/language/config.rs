// Maybe for future
enum Semantics {
    WellFounded,
    StableModel,
}

// Modal logic, change semantics on the fly
struct Config {
    use_subconsequents: bool,
    semantics: Semantics,
    max_iterations: usize,
}

impl Config {
    /*
    pub fn set(&self, feature: &str, value: &str) -> Result<(), ()> {
        match feature {
            "subconsequents" => {}
            "semantics" =>
            "max_iterations" => self.max_iterations == usize::from_str_radix(value, 0)?,
            _ => Err(()),
        }
    }
    */
}
