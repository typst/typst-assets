//! Data describing HTML elements and attributes and how the attributes
//! map to Typst types.

/// Details about an HTML element.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ElemInfo {
    /// The element's tag name.
    pub name: &'static str,
    /// A description for the element.
    pub docs: &'static str,
    /// Indices of the element's attributes in `ATTRS`.
    pub attrs: &'static [u8],
}

impl ElemInfo {
    /// Creates element information from its parts.
    ///
    /// The `attrs` slice consists of indices pointing into `generated::ATTRS`.
    /// It must be sorted by index (and, by extension, also by name of the
    /// pointed-to attributes because the attributes themselves are sorted).
    pub const fn new(name: &'static str, docs: &'static str, attrs: &'static [u8]) -> Self {
        Self { name, docs, attrs }
    }

    /// Iterates over all attributes an element of this type can have
    /// (both specific and global).
    pub fn attributes(&self) -> impl Iterator<Item = &'static AttrInfo> {
        self.attrs
            .iter()
            .map(|&i| &ATTRS[usize::from(i)])
            .chain(&ATTRS[..ATTRS_GLOBAL])
    }

    /// Provides metadata for an attribute with the given name if it exists for
    /// this element. The attribute may be specific or global.
    pub fn get_attr(&self, name: &str) -> Option<&'static AttrInfo> {
        self.get_specific_attr(name)
            .or_else(|| self.get_global_attr(name))
            .map(|i| &ATTRS[i])
    }

    /// Tries to locate the index of a specific attribute in `ATTRS`.
    fn get_specific_attr(&self, name: &str) -> Option<usize> {
        self.attrs
            .binary_search_by_key(&name, |&i| ATTRS[usize::from(i)].name)
            .map(|k| usize::from(self.attrs[k]))
            .ok()
    }

    /// Tries to locate the index of a global attribute in `ATTRS`.
    fn get_global_attr(&self, name: &str) -> Option<usize> {
        ATTRS[..ATTRS_GLOBAL]
            .binary_search_by_key(&name, |attr| attr.name)
            .ok()
    }
}

/// Details about an HTML attribute.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct AttrInfo {
    /// The attribute's name.
    pub name: &'static str,
    /// A description for the attribute.
    pub docs: &'static str,
    /// Type information for the attribute.
    pub ty: Type,
}

impl AttrInfo {
    /// Creates attribute information from its parts.
    pub const fn new(name: &'static str, docs: &'static str, ty: Type) -> Self {
        Self { name, docs, ty }
    }
}

/// Defines how an attribute maps to Typst types.
///
/// Each variant's documentation describes which Typst type is accepted and into
/// what kind of HTML attribute string it is converted.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    /// `bool` → attribute is present or absent.
    Presence,
    /// `none` → `"none"`.
    None,
    /// `none` → `""`.
    NoneEmpty,
    /// `none` → `"undefined"`.
    NoneUndefined,
    /// `auto` → `"auto"`.
    Auto,
    /// `boolean` → `"true"` or `"false"`.
    TrueFalse,
    /// `boolean` → `"yes"` or `"no"`.
    YesNo,
    /// `boolean` → `"on"` or `"off"`.
    OnOff,
    /// `ltr` or `rtl` → `"ltr"` or `"rtl"`.
    HorizontalDir,
    /// `string` → string.
    Str,
    /// `char` → string.
    Char,
    /// `int` → stringified int.
    Int,
    /// `int >= 0` → stringified int.
    NonNegativeInt,
    /// `int > 0` → stringified int.
    PositiveInt,
    /// `float` → stringified float.
    Float,
    /// `float` > 0 → stringified float.
    PositiveFloat,
    /// `datetime` → stringified datetime.
    Datetime,
    /// `duration` → stringified duration.
    Duration,
    /// `color` → stringified CSS color.
    Color,
    /// `array (w, h)` of two `int >= 0` → `"{w}x{h}"`.
    IconSize,
    /// `dictionary` with keys `src` (string) and optionally `width` (int) or
    /// `density` (positive float) → `"{src}"` plus optionally space-separated
    /// `"{width}w"` or `{density}d`.
    ImageCandidate,
    /// `dictionary` with keys `condition` (string) and `size` (length) →
    /// `"({condition}) {size}"`
    SourceSize,
    /// For `Strings(a, b)`, any of the strings in `ATTR_STRINGS[a, b]` →
    /// string.
    Strings(usize, usize),
    /// Any of the listed types → the respective output.
    Union(&'static [Type]),
    /// An array of the listed type, or, if the bool is true, the listed type
    /// itself as a shorthand → a string containing the respective outputs
    /// separated by the char.
    List(&'static Type, char, bool),
}

#[path = "../files/html/data.rs"]
mod data;

pub use data::*;
