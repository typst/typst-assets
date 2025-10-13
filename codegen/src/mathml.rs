//! Usage: `cargo run -p typst-assets-codegen -- mathml path/to/spec/dir`
//!
//! The spec dir will automatically be populated with the necessary
//! specifications if one is missing.

use std::fmt::Write;

use crate::*;
use regex::Regex;
use scraper::{ElementRef, Selector};

// When referencing more specs, make sure to update the NOTICE file.
#[rustfmt::skip]
mod spec {
    pub const LIST: &[&str] = &[OPERATOR_DICTIONARY, INLINE_AXIS_OPERATORS];
    pub const OPERATOR_DICTIONARY: &str = "https://raw.githubusercontent.com/w3c/mathml-core/refs/heads/main/tables/operator-dictionary-compact.html";
    pub const INLINE_AXIS_OPERATORS: &str = "https://raw.githubusercontent.com/w3c/mathml-core/refs/heads/main/tables/inline-axis-operators.html";
}

pub fn main() {
    // Directory where specs will be read from / downloaded into.
    let dir = std::env::args_os().nth(2).map(std::path::PathBuf::from);
    let ctx = Context {
        operator_dictionary: load_spec(&dir, spec::OPERATOR_DICTIONARY),
        inline_axis_operators: load_spec(&dir, spec::INLINE_AXIS_OPERATORS),
    };

    let operators = collect_operators(&ctx);
    let inline_stretch_axis = collect_inline_stretch_axis(&ctx);
    let special_tables = collect_special_tables(&ctx);
    let categories = collect_categories(&ctx);

    let output = Output {
        operators,
        inline_stretch_axis,
        special_tables,
        categories,
    };

    let path = "files/mathml/data.rs";
    let code = codegen(&output);
    std::fs::write(path, code).unwrap();

    eprintln!("Success!");
}

struct Context<'a> {
    operator_dictionary: ElementRef<'a>,
    inline_axis_operators: ElementRef<'a>,
}

struct Output {
    operators: Vec<OperatorInfo>,
    inline_stretch_axis: InlineStretchAxis,
    special_tables: SpecialTables,
    categories: Vec<CategoryInfo>,
}

struct CategoryInfo {
    name: String,
    form: Option<String>,
    encoding: Option<u8>,
    lspace: f64,
    rspace: f64,
    properties: Vec<String>,
}

/// Parses operators values for each category from MathML Core Figure 26.
fn collect_categories(ctx: &Context) -> Vec<CategoryInfo> {
    let mut categories = vec![];
    for tr in ctx
        .operator_dictionary
        .select(s!(
            "#operator-dictionary-categories-values > table > tbody > tr"
        ))
        .skip(1)
    {
        let to_option = |text| (text != "N/A").then_some(text);
        let to_f64 = |text: String| text.trim_end_matches("em").parse::<f64>().unwrap();

        let form = to_option(tr.select_text(s!("td:nth-of-type(2)"))).map(|text| {
            let mut c = text.chars();
            c.next().unwrap().to_uppercase().collect::<String>() + c.as_str()
        });

        let encoding = to_option(tr.select_text(s!("td:nth-of-type(3)")))
            .map(|text| text.trim_start_matches("0x").to_owned())
            .map(|text| u8::from_str_radix(&text, 16).unwrap());

        let mut properties = vec![];
        if let Some(props) = to_option(tr.select_text(s!("td:nth-of-type(6)"))) {
            props
                .split_ascii_whitespace()
                .for_each(|prop| properties.push(prop.to_owned()));
        }

        categories.push(CategoryInfo {
            name: tr.select_text(s!("td:nth-of-type(1)")),
            form,
            encoding,
            lspace: to_f64(tr.select_text(s!("td:nth-of-type(4) > code"))),
            rspace: to_f64(tr.select_text(s!("td:nth-of-type(5) > code"))),
            properties,
        });
    }
    categories
}

struct OperatorInfo {
    key: u16,
    category: u8,
    length: u8,
}

/// Parses list of entries for the largest categories, sorted by key, from
/// MathML Core Figure 27.
fn collect_operators(ctx: &Context) -> Vec<OperatorInfo> {
    ctx.operator_dictionary
        .select_text(s!("#operator-dictionary-categories-hexa-table > code"))
        .split_terminator(", ")
        .map(|entry| entry.replace_regex(re!(r"[\[\]{}]|0x"), ""))
        .map(|value| {
            let (start, length) = match value.split_once('–') {
                None => (u16::from_str_radix(&value, 16).unwrap(), 1u8),
                Some((start, end)) => {
                    let start = u16::from_str_radix(start, 16).unwrap();
                    let end = u16::from_str_radix(end, 16).unwrap();
                    (start, (end - start + 1) as u8)
                }
            };
            OperatorInfo {
                key: start % 0x4000,
                category: (start / 0x1000) as u8,
                length,
            }
        })
        .collect()
}

struct InlineStretchAxis {
    bmp: Vec<u16>,
    non_bmp: Vec<char>,
}

/// Parses sorted list of Unicode code points corresponding to operators with
/// inline stretch axis from MathML Core Figure 28.
fn collect_inline_stretch_axis(ctx: &Context) -> InlineStretchAxis {
    let mut bmp = vec![];
    let mut non_bmp = vec![];
    for codepoint in ctx
        .inline_axis_operators
        .select_text(s!("#inline-stretch-axis > code"))
        .split_terminator(",\n")
    {
        let c = parse_char(codepoint);
        match u16::try_from(c) {
            Ok(value) => bmp.push(value),
            Err(_) => non_bmp.push(c),
        }
    }
    InlineStretchAxis { bmp, non_bmp }
}

struct Fence {
    c: char,
    length: u8,
}

struct SpecialTables {
    two_ascii_chars: Vec<String>,
    fence: Vec<Fence>,
    separator: Vec<char>,
}

/// Parses special tables for the operator dictionary from MathML Core Figure
/// 24.
fn collect_special_tables(ctx: &Context) -> SpecialTables {
    let table = ctx.operator_dictionary.select_first(s!(
        "#operator-dictionary-compact-special-tables > table > tbody"
    ));
    let two_ascii_chars = table
        .select_text(s!("tr:nth-of-type(2) > td:nth-of-type(2) > code"))
        .split_terminator(", ")
        .map(|text| text.trim_matches('\'').to_owned())
        .collect();
    let fence = table
        .select_text(s!("tr:nth-of-type(3) > td:nth-of-type(2) > code"))
        .split_terminator(", ")
        .map(|entry| entry.replace_regex(re!(r"[\[\]{}]"), ""))
        .map(|value| {
            let (c, length) = match value.split_once('–') {
                None => (parse_char(&value), 1u8),
                Some((start, end)) => {
                    let start = parse_char(start);
                    let end = parse_char(end);
                    (start, (end as u32 - start as u32 + 1) as u8)
                }
            };
            Fence { c, length }
        })
        .collect();

    let separator = table
        .select_text(s!("tr:nth-of-type(4) > td:nth-of-type(2) > code"))
        .split_terminator(", ")
        .map(parse_char)
        .collect();
    SpecialTables {
        two_ascii_chars,
        fence,
        separator,
    }
}

fn parse_char(codepoint: &str) -> char {
    codepoint
        .strip_prefix("U+")
        .and_then(|x| u32::from_str_radix(x, 16).ok())
        .and_then(|x| char::from_u32(x))
        .unwrap()
}

/// Generates the output file.
fn codegen(output: &Output) -> String {
    let categories = output.categories.iter().map(|info| {
       let CategoryInfo {
           name,
           form,
           lspace,
           rspace,
           properties, ..
       } = info;
       let props = properties.iter().map(|prop| match prop.as_str() {
            "stretchy" => "Properties::STRETCHY.bits()",
            "symmetric" => "Properties::SYMMETRIC.bits()",
            "largeop" => "Properties::LARGEOP.bits()",
            "movablelimits" => "Properties::MOVABLELIMITS.bits()",
            _ => "",
       }).filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" | ");
       format!(
           "static CATEGORY_{}: OperatorInfo = OperatorInfo {{ \n    form: {},\n    lspace: {:?},\n    rspace: {:?},\n    properties: {},\n}};",
           name.to_uppercase(), form.clone().map_or("None".to_owned(), |x| format!("Some(Form::{x})")), lspace, rspace,
           if props.is_empty() { "Properties::empty()".to_owned() } else { format!("Properties::from_bits_retain({props})" ) },
       )
    });
    let category = output
        .categories
        .iter()
        .map(|info| format!("    {},", info.name));
    let encoding = output
        .categories
        .iter()
        .filter(|info| info.encoding.is_some())
        .map(|info| {
            format!(
                "        0x{:02x} => Category::{},",
                info.encoding.unwrap(),
                info.name
            )
        });
    let category_map = output.categories.iter().map(|info| {
        format!(
            "        Category::{} => &CATEGORY_{},",
            info.name,
            info.name.to_uppercase()
        )
    });

    let operators = output.operators.iter().map(|entry| {
        format!(
            "    (0x{:04x}, 0x{:02x}),",
            entry.key,
            (entry.category << 4) | (entry.length & 0x0F),
        )
    });

    let two_ascii_chars = output
        .special_tables
        .two_ascii_chars
        .iter()
        .map(|s| format!("    \"{s}\","));
    let fence = output
        .special_tables
        .fence
        .iter()
        .map(|f| format!("    ('\\u{{{:X}}}', 0x{:02x}),", f.c as u32, f.length));
    let separator = output
        .special_tables
        .separator
        .iter()
        .map(|c| format!("'\\u{{{:X}}}'", *c as u32));

    let inline_axis_bmp = output
        .inline_stretch_axis
        .bmp
        .iter()
        .map(|i| format!("    0x{i:04x},"));
    let inline_axis_non_bmp = output
        .inline_stretch_axis
        .non_bmp
        .iter()
        .map(|c| format!("'\\u{{{:X}}}'", *c as u32));

    let mut out = String::new();
    macro_rules! w {
        ($($tts:tt)*) => {
            writeln!(out,  $($tts)*).unwrap();
        }
    }

    w!(
        "// This file is generated by `{}` and derived from:",
        file!()
    );
    w!("//");
    for spec in spec::LIST {
        w!("// - {spec}");
    }
    w!("//");
    w!("// Do not edit by hand.");
    w!();
    w!("#![cfg_attr(rustfmt, rustfmt_skip)]");
    w!();
    w!("use super::{{Form, Properties, OperatorInfo}};");
    w!();
    w!("{}", categories.join("\n\n"));
    w!();
    w!("#[derive(Debug, Copy, Clone, Eq, PartialEq)]");
    w!("pub(crate) enum Category {{");
    w!("{}", category.join("\n"));
    w!("}}");
    w!();
    w!("pub(crate) fn from_encoding(encoding: u8) -> Category {{");
    w!("    match encoding {{");
    w!("{}", encoding.join("\n"));
    w!("        _ => unreachable!(),");
    w!("    }}");
    w!("}}");
    w!();
    w!("pub(crate) fn get_category_info(category: Category) -> &'static OperatorInfo {{");
    w!("    match category {{");
    w!("{}", category_map.join("\n"));
    w!("    }}");
    w!("}}");
    w!();
    w!("pub(crate) const OPERATOR_TABLE: &[(u16, u8)] = &[");
    w!("{}", operators.join("\n"));
    w!("];");
    w!();
    w!("pub(crate) const TWO_ASCII_CHARS_TABLE: &[&str] = &[");
    w!("{}", two_ascii_chars.join("\n"));
    w!("];");
    w!();
    w!("pub(crate) const FENCE_TABLE: &[(char, u8)] = &[");
    w!("{}", fence.join("\n"));
    w!("];");
    w!();
    w!(
        "pub(crate) const SEPARATOR_TABLE: &[char] = &[{}];",
        separator.join(", ")
    );
    w!();
    w!("pub(crate) const INLINE_AXIS_BMP_TABLE: &[u16] = &[");
    w!("{}", inline_axis_bmp.join("\n"));
    w!("];");
    w!();
    w!(
        "pub(crate) const INLINE_AXIS_NON_BMP_TABLE: &[char] = &[{}];",
        inline_axis_non_bmp.join(", ")
    );

    out
}
