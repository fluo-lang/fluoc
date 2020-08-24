
#[derive(Clone, Copy, PartialEq, Debug)]
/// Unit level tags
pub struct UnitTags {
    pub no_std: bool,
    pub no_core: bool,
}

impl UnitTags {
    pub fn new() -> Self {
        UnitTags {
            no_std: false,
            no_core: false,
        }
    }
}

