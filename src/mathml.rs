//! Data describing an operator's properties from the MathML Core operator
//! dictionary.

use std::cmp::Ordering;

use bitflags::bitflags;

/// The form an operator can have.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Form {
    Infix,
    Prefix,
    Postfix,
}

bitflags! {
    /// Set of properties an operator can have.
    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    pub struct Properties: u8 {
        const STRETCHY      = 1 << 0;
        const SYMMETRIC     = 1 << 1;
        const LARGEOP       = 1 << 2;
        const MOVABLELIMITS = 1 << 3;
    }
}

/// Information about an operator.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct OperatorInfo {
    pub form: Option<Form>,
    pub lspace: f64,
    pub rspace: f64,
    pub properties: Properties,
}

#[path = "../files/mathml/data.rs"]
mod data;

/// Gets the `[OperatorInfo]` struct corresponding to the given (Content, Form)
/// pair.
pub fn get_operator_info(content: &str, form: Form) -> &'static OperatorInfo {
    let category = get_operator_category(content, form);
    data::get_category_info(category)
}

fn get_operator_category(content: &str, form: Form) -> data::Category {
    use data::*;

    let mut chars = content.chars();
    let lookup = if let Some(first) = chars.next() {
        if let Some(second) = chars.next() {
            // Step 1.
            if chars.next().is_some() {
                return Category::Default;
            }

            // Step 2.2.
            if matches!(second, '\u{0338}' | '\u{20D2}') {
                first

            // Step 2.3.
            } else if let Some(idx) = TWO_ASCII_CHARS_TABLE.iter().position(|&s| s == content) {
                println!("{:X}", 0x0320 + idx as u32);
                char::from_u32(0x0320 + idx as u32).unwrap()

            // Step 2.4.
            } else {
                return Category::Default;
            }

        // Step 2.
        } else if ('\u{0320}'..='\u{03FF}').contains(&first) {
            return Category::Default;

        // Step 2.1.
        } else if matches!(first, '\u{1EEF0}' | '\u{1EEF1}') && form == Form::Postfix {
            return Category::I;
        } else {
            first
        }

    // Step 1.
    } else {
        return Category::Default;
    };

    // Step 3.
    if form == Form::Infix && matches!(lookup, '\u{007C}' | '\u{223C}') {
        return Category::ForceDefault;
    }
    match (lookup, form) {
        ('\u{2145}'..='\u{2146}' | '\u{2202}' | '\u{221A}'..='\u{221C}', Form::Prefix) => {
            return Category::L;
        }
        ('\u{002C}' | '\u{003A}' | '\u{003B}', Form::Infix) => {
            return Category::M;
        }
        _ => {}
    }

    // Step 3.1.
    let key = match lookup as u32 {
        0x0000..=0x03FF => lookup as u16,
        0x2000..=0x2BFF => (lookup as u32 - 0x1C00) as u16,
        _ => return Category::Default,
    };

    // Step 3.2.
    let key = key
        + match form {
            Form::Infix => 0x0000,
            Form::Prefix => 0x1000,
            Form::Postfix => 0x2000,
        };

    // Step 3.3.
    assert!(key <= 0x2FFF);

    // Step 3.4.
    OPERATOR_TABLE
        .binary_search_by(|&(k, data)| {
            if key < k {
                Ordering::Greater
            } else if key >= k + (data & 0x0F) as u16 {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        })
        .map_or(Category::Default, |idx| {
            from_encoding(OPERATOR_TABLE[idx].1 >> 4)
        })
}

/// Whether the given text has the fence attribute.
pub fn is_fence(content: &str) -> bool {
    match content.parse::<char>() {
        Ok(c) => data::FENCE_TABLE
            .binary_search_by(|&(s, len)| {
                if c < s {
                    Ordering::Greater
                } else if c as u32 >= s as u32 + len as u32 {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            })
            .is_ok(),
        Err(_) => false,
    }
}

/// Whether the given text has the separator attribute.
pub fn is_separator(content: &str) -> bool {
    match content.parse::<char>() {
        Ok(c) => data::SEPARATOR_TABLE.contains(&c),
        Err(_) => false,
    }
}

/// Whether the intrinsic stretch axis of the given Unicode character is
/// inline. If it is not inline, then it is block.
pub fn stretch_axis_is_inline(c: char) -> bool {
    if let Ok(value) = u16::try_from(c) {
        data::INLINE_AXIS_BMP_TABLE.binary_search(&value).is_ok()
    } else {
        data::INLINE_AXIS_NON_BMP_TABLE.contains(&c)
    }
}

/// Whether the given text is a single character and would be converted to its
/// italic variant if the `math-auto` property is present.
pub fn will_auto_transform(content: &str) -> bool {
    let mut chars = content.chars();
    if let Some(c) = chars.next()
        && chars.next().is_none()
        && matches!(c, 'A'..='Z' | 'a'..='z' | 'ı' | 'ȷ' | 'Α'..='Ρ' | 'ϴ' | 'Σ'..='Ω' | '∇' | 'α'..='ω' | '∂' | 'ϵ' | 'ϑ' | 'ϰ' | 'ϕ' | 'ϱ' | 'ϖ')
    {
        return true;
    }
    false
}

#[cfg(test)]
mod tests {
    use super::data::*;
    use super::*;

    #[test]
    fn test_get_operator_info() {
        assert_eq!(get_operator_category("", Form::Infix), Category::Default);
        assert_eq!(get_operator_category("abc", Form::Infix), Category::Default);
        assert_eq!(get_operator_category("+", Form::Infix), Category::B);
        assert_eq!(get_operator_category("+", Form::Prefix), Category::D);
        assert_eq!(get_operator_category("→", Form::Infix), Category::A);
        assert_eq!(get_operator_category("∑", Form::Prefix), Category::J);
        assert_eq!(get_operator_category("!", Form::Postfix), Category::E);
        assert_eq!(
            get_operator_category("|", Form::Infix),
            Category::ForceDefault
        );
        assert_eq!(
            get_operator_category("∼", Form::Infix),
            Category::ForceDefault
        );
        assert_eq!(get_operator_category("|", Form::Prefix), Category::F);
        assert_eq!(get_operator_category("√", Form::Prefix), Category::L);
        assert_eq!(get_operator_category(",", Form::Infix), Category::M);
        assert_eq!(get_operator_category("a", Form::Infix), Category::Default);
        assert_eq!(get_operator_category("1", Form::Infix), Category::Default);
        assert_eq!(
            get_operator_category("\u{0320}", Form::Infix),
            Category::Default
        );
        assert_eq!(
            get_operator_category("\u{1EEF0}", Form::Postfix),
            Category::I
        );
        assert_eq!(
            get_operator_category("\u{1EEF0}", Form::Infix),
            Category::Default
        );
        assert_eq!(get_operator_category("/\u{0338}", Form::Infix), Category::K);
        assert_eq!(get_operator_category("&&", Form::Infix), Category::B);
        assert_eq!(get_operator_category("!!", Form::Postfix), Category::E);
        assert_eq!(get_operator_category("||", Form::Infix), Category::Default);
        assert_eq!(get_operator_category("||", Form::Prefix), Category::D);
        assert_eq!(get_operator_category("xx", Form::Infix), Category::Default);
    }

    #[test]
    fn test_fence() {
        assert_eq!(is_fence("\u{201D}"), true);
        assert_eq!(is_fence("\u{27E7}"), true);
        assert_eq!(is_fence("\u{0331}"), true);
        assert_eq!(is_fence("\u{29D8}"), true);
        assert_eq!(is_fence("\u{232B}"), false);
        assert_eq!(is_fence("\u{2015}"), false);
        assert_eq!(is_fence("=="), false);
        assert_eq!(is_fence(""), false);
    }

    #[test]
    fn test_separator() {
        assert_eq!(is_separator("\u{002C}"), true);
        assert_eq!(is_separator("\u{003B}"), true);
        assert_eq!(is_separator("\u{2063}"), true);
        assert_eq!(is_separator("\u{2064}"), false);
        assert_eq!(is_separator("\u{29D8}"), false);
        assert_eq!(is_separator(""), false);
        assert_eq!(is_separator("!="), false);
    }

    #[test]
    fn test_stretch_axis_is_inline() {
        assert_eq!(stretch_axis_is_inline('\u{21CF}'), true);
        assert_eq!(stretch_axis_is_inline('\u{1EEF0}'), true);
        assert_eq!(stretch_axis_is_inline('\u{1EEF1}'), true);
        assert_eq!(stretch_axis_is_inline('\u{FE38}'), true);
        assert_eq!(stretch_axis_is_inline('\u{003D}'), true);
        assert_eq!(stretch_axis_is_inline('\u{295C}'), false);
        assert_eq!(stretch_axis_is_inline('\u{1D11E}'), false);
        assert_eq!(stretch_axis_is_inline('\u{1EEF2}'), false);
        assert_eq!(stretch_axis_is_inline('\u{003C}'), false);
    }
}
