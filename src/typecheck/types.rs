use super::mir;
use crate::helpers;

pub struct Tuple {
    types: Vec<MirType>,
    pos: helpers::Pos,
}

pub enum MirType {
    /// Tuple types, E.g., (int, int), (str, my::type)
    Tuple(Tuple),
    /// Namespace types. E.g., str, int, my::type
    Namespace(mir::Namespace),
}
