//! Usage: `cargo run -p typst-assets-codegen -- html path/to/spec/dir`
//!
//! The spec dir will automatically be populated with the necessary
//! specifications if one is missing.

use std::borrow::Cow;
use std::fmt::{Display, Write};
use std::path::PathBuf;

use regex::Regex;
use scraper::{ElementRef, Html, Selector};

pub fn main() {
    // Directory where specs will be read from / downloaded into.
    let dir = std::env::args_os().nth(2).map(PathBuf::from);
    let mut ctx = Context {
        html: load_spec(&dir, "html", "https://html.spec.whatwg.org/"),
        fetch: load_spec(&dir, "fetch", "https://fetch.spec.whatwg.org/"),
        referrer: load_spec(
            &dir,
            "referrer",
            "https://w3c.github.io/webappsec-referrer-policy/",
        ),
        aria: load_spec(&dir, "aria", "https://www.w3.org/TR/wai-aria-1.1/"),
        strings: vec![],
    };

    let attr_infos = collect_attributes(&mut ctx);
    let element_infos = collect_elements(&ctx, &attr_infos);
    let attr_global_count = attr_infos
        .iter()
        .filter(|info| matches!(info.applies_to, Applicable::Globally))
        .count();

    let output = Output {
        element_infos,
        attr_infos,
        attrs_global: attr_global_count,
        attr_strings: ctx.strings,
    };

    let path = "files/html/data.rs";
    let code = codegen(&output);
    std::fs::write(path, code).unwrap();

    println!("Success!");
}

/// Reads a spec from the directory or, if it does not exist, fetches and stores it.
fn load_spec(spec_dir: &Option<PathBuf>, name: &str, url: &str) -> ElementRef<'static> {
    let text = if let Some(dir) = spec_dir {
        let path = dir.join(name).with_extension("html");
        if path.exists() {
            println!("Reading from {}", path.display());
            std::fs::read_to_string(&path).unwrap()
        } else {
            let text = crate::fetch(url);
            println!("Writing to {}", path.display());
            std::fs::create_dir_all(dir).unwrap();
            std::fs::write(&path, &text).unwrap();
            text
        }
    } else {
        crate::fetch(url)
    };
    Box::leak(Box::new(Html::parse_document(&text))).root_element()
}

struct Context<'a> {
    html: ElementRef<'a>,
    fetch: ElementRef<'a>,
    referrer: ElementRef<'a>,
    aria: ElementRef<'a>,
    strings: Vec<(String, String)>,
}

struct Output {
    element_infos: Vec<ElementInfo>,
    attr_infos: Vec<AttrInfo>,
    attrs_global: usize,
    attr_strings: Vec<(String, String)>,
}

struct ElementInfo {
    name: String,
    docs: String,
    attributes: Vec<usize>,
}

struct AttrInfo {
    name: String,
    docs: String,
    ty: Type,
    applies_to: Applicable,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum Type {
    Presence,
    None,
    NoneEmpty,
    NoneUndefined,
    Auto,
    TrueFalse,
    YesNo,
    OnOff,
    Int,
    NonNegativeInt,
    PositiveInt,
    Float,
    PositiveFloat,
    Str,
    Char,
    Datetime,
    Duration,
    Color,
    HorizontalDir,
    IconSize,
    ImageCandidate,
    SourceSize,
    Strings(usize, usize),
    Union(Vec<Type>),
    List(Box<Type>, char, bool),
}

impl Type {
    fn union_(iter: impl IntoIterator<Item = Type>) -> Type {
        Type::Union(iter.into_iter().collect())
    }

    fn list(ty: Type, sep: char) -> Type {
        Type::List(Box::new(ty), sep, true)
    }

    fn list_without_shorthand(ty: Type, sep: char) -> Type {
        Type::List(Box::new(ty), sep, false)
    }

    fn encode(&self) -> String {
        match self {
            Type::Union(types) => format!(
                "Type::Union(&[{}])",
                types
                    .iter()
                    .map(Self::encode)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::List(ty, c, b) => format!("Type::List(&{}, {c:?}, {b:?})", ty.encode()),
            _ => format!("Type::{:?}", self),
        }
    }
}

enum Applicable {
    Globally,
    Elements(Vec<String>),
}

impl Applicable {
    fn applies_specifically_to(&self, tag: &str) -> bool {
        match self {
            Self::Globally => false,
            Self::Elements(elements) => elements.iter().any(|s| s == tag),
        }
    }
}

/// Creates a lazily initialized static value.
macro_rules! lazy {
    ($ty:ty = $init:expr) => {{
        static VAL: ::std::sync::LazyLock<$ty> = ::std::sync::LazyLock::new(|| $init);
        &*VAL
    }};
}

/// Creates a static CSS selector.
macro_rules! s {
    ($s:literal) => {
        lazy!(Selector = Selector::parse($s).unwrap())
    };
}

/// Creates a lazily initialized regular expression.
macro_rules! re {
    ($s:expr) => {
        lazy!(Regex = Regex::new($s).unwrap())
    };
}

/// Like `match`, but with regular expressions!
macro_rules! regex_match {
    ($text:expr, {
        $($re:literal $(if $guard:expr)? => $out:expr,)*
        _ => $final:expr $(,)?
    }) => {{
        let __text = $text;
        match () {
            $(_ if re!(&concat!("(?i)^", $re, "$")).is_match(__text)
                $(&& $guard)? => $out,)*
            _ => $final
        }
    }};
}

/// Collects all attributes with documentation and descriptions.
fn collect_attributes(ctx: &mut Context) -> Vec<AttrInfo> {
    let mut infos = vec![];
    collect_html_attributes(ctx, &mut infos);
    collect_aria_attributes(ctx, &mut infos);
    infos.sort_by(|a, b| sort_key(a).cmp(&sort_key(b)));
    infos
}

/// Global attributes should come first and attributes be binary-searchable.
fn sort_key(attr: &AttrInfo) -> impl Ord + '_ {
    (
        matches!(attr.applies_to, Applicable::Elements(_)),
        &attr.name,
    )
}

/// Collects attributes from the HTML spec.
fn collect_html_attributes(ctx: &mut Context, infos: &mut Vec<AttrInfo>) {
    for tr in ctx
        .html
        .select_first(s!("#attributes-1"))
        .select(s!("tbody > tr"))
    {
        let name = tr.select_text(s!("th code"));
        let elements = tr.select_first(s!("td:nth-of-type(1)"));
        let mut docs = docs(&tr.select_text(s!("td:nth-of-type(2)")));

        let applies_to = if elements.inner_text().trim() == "HTML elements" {
            Applicable::Globally
        } else {
            Applicable::Elements(
                elements
                    .select(s!("code"))
                    .map(|elem| elem.inner_text())
                    .collect(),
            )
        };

        let ty_cell = tr.select_first(s!("td:nth-of-type(3)"));
        let ty = determine_type(ctx, &name, ty_cell, &mut docs);
        infos.push(AttrInfo {
            name,
            docs,
            ty,
            applies_to,
        });
    }

    // HTML spec is missing this.
    infos.push(AttrInfo {
        name: "rel".into(),
        docs: "Relationship between the document containing \
               the form and its action destination"
            .into(),
        ty: rel_type(ctx),
        applies_to: Applicable::Elements(vec!["form".into()]),
    });
}

/// Collects attributes from the ARIA spec.
fn collect_aria_attributes(ctx: &mut Context, infos: &mut Vec<AttrInfo>) {
    // Collect ARIA roles.
    let role_dl = ctx.aria.select_first(s!("#index_role"));
    infos.push(AttrInfo {
        name: "role".into(),
        docs: "An ARIA role.".into(),
        ty: create_str_enum(
            ctx,
            role_dl
                .select(s!("dt code"))
                .zip(role_dl.select(s!("dd")))
                .map(|(code, dd)| (code.inner_text(), dd.inner_text())),
        ),
        applies_to: Applicable::Globally,
    });

    // Collect ARIA property and state attributes.
    let attrs_dl = ctx.aria.select_first(s!("#index_state_prop"));
    for (dt, dd) in attrs_dl.select(s!("dt")).zip(attrs_dl.select(s!("dd"))) {
        let docs = docs(&dd.inner_text());
        if docs.contains("Deprecated") {
            continue;
        }

        let name = dt.inner_text();
        let ty = determine_aria_type(ctx, &name);
        infos.push(AttrInfo {
            name,
            docs,
            ty,
            applies_to: Applicable::Globally,
        });
    }
}

/// Collects all HTML elements.
fn collect_elements(ctx: &Context, attrs: &[AttrInfo]) -> Vec<ElementInfo> {
    let mut infos = vec![];
    for tr in ctx
        .html
        .select_first(s!("#elements-3 ~ table"))
        .select(s!("tbody > tr"))
    {
        for code in tr.select(s!("th code")) {
            let name = code.inner_text();

            // These are special and not normal HTML elements.
            if matches!(name.as_str(), "svg" | "math") {
                continue;
            }

            let docs = docs(&tr.select_text(s!("td:first-of-type")));
            let attributes = collect_attr_indices(tr, &name, attrs);

            infos.push(ElementInfo {
                name,
                docs,
                attributes,
            });
        }
    }
    infos
}

/// Collects the indices of the attribute infos for an element's attributes.
fn collect_attr_indices(tr: ElementRef, tag: &str, attrs: &[AttrInfo]) -> Vec<usize> {
    let mut indices = vec![];
    for elem in tr.select(s!("td:nth-of-type(5) code")) {
        let name = elem.inner_text();

        // Ignore the event handle attributes on the body element that are
        // for some reason documented (unlike other event handle attributes).
        if tag == "body" && name.starts_with("on") {
            continue;
        }

        let index = attrs
            .iter()
            .position(|attr| attr.name == name && attr.applies_to.applies_specifically_to(tag))
            .unwrap_or_else(|| panic!("failed to find attribute {name} for {tag}"));

        indices.push(index)
    }
    indices.sort();
    assert!(indices.is_sorted_by_key(|&i| &attrs[i].name));
    indices
}

/// Determines the Rust type for an HTML attribute.
fn determine_type(ctx: &mut Context, attr: &str, cell: ElementRef, docs: &mut String) -> Type {
    let textual_ty = cell
        .inner_text()
        .trim()
        .trim_end_matches("*")
        .replace("\n", " ");
    if let Some(ty) = try_parse_alternation(ctx, &textual_ty) {
        return ty;
    }

    regex_match!(textual_ty.as_str(), {
        "autofill field name.*" => Type::Str,
        "css declarations" => Type::Str,
        "id" => Type::Str,
        "regular expression.*" => Type::Str,
        "serialized permissions policy" => Type::Str,
        "text" => Type::Str,
        "the source of an iframe srcdoc document" => Type::Str,
        "valid (non-empty )?url.*" => Type::Str,
        "valid bcp 47 language tag" => Type::Str,
        "valid custom element name.*" => Type::Str,
        "valid hash-name reference" => Type::Str,
        "valid mime type string" => Type::Str,
        "valid integer" => Type::Int,
        "valid non-negative integer" => Type::NonNegativeInt,
        "valid non-negative integer greater than zero" => Type::PositiveInt,
        "valid floating-point number" => Type::Float,
        "valid float.* greater than zero, or \"any\"" => {
            Type::union_([Type::PositiveFloat,  create_str_literal(ctx, "any")])
        },
        "css <color>" => Type::Color,
        "valid date string with optional time" => Type::Datetime,
        "valid month string.*valid duration string" => Type::union_([Type::Duration, Type::Datetime]),
        "boolean attribute" => Type::Presence,
        "valid bcp 47 language tag or the empty string" => Type::union_([Type::Str, Type::NoneEmpty]),
        ".*until-found.*" if attr == "hidden" => {
            Type::union_([Type::Presence, create_str_literal(ctx, "until-found")])
        },
        ".*true.*empty string.*" if matches!(attr, "spellcheck" | "writingsuggestions") => Type::Presence,
        "valid list of floating-point numbers" => {
            write!(docs, " Expects an array of floating point numbers.").unwrap();
            Type::list_without_shorthand(Type::Float, ',')
        },
        ".*space-separated tokens.*" if attr == "rel" => rel_type(ctx),
        ".*space-separated tokens.*" if attr == "sandbox" => {
            let variants = cell
                .select(s!("code"))
                .map(|elem| elem.inner_text());
            let ty = create_str_enum(ctx, variants);
            Type::list(ty, ' ')
        },
        ".*space-separated tokens.*consisting of one code point.*" => {
            write!(docs, " Expects a single-codepoint string or an array thereof.").unwrap();
            Type::list(Type::Char, ',')
        },
        ".*space-separated tokens.*consisting of sizes" => {
            write!(
                docs,
                " Expects an array of sizes. Each size is specified as an \
                  array of two integers (width and height).",
            ).unwrap();
            Type::list_without_shorthand(Type::IconSize, ' ')
        },
        ".*space-separated tokens.*" => Type::list(Type::Str, ' '),
        ".*comma-separated tokens.*" => Type::list(Type::Str, ','),
        "valid media query list" => Type::Str,
        "ascii case-insensitive match for \"utf-8\"" => create_str_literal(ctx, "utf-8"),
        "varies" if matches!(attr, "min" | "max") => Type::union_([Type::Str, Type::Float, Type::Datetime]),
        "varies" if attr == "value" => Type::union_([Type::Str, Type::Float, Type::Datetime, Type::Color, Type::list_without_shorthand(Type::Str, ',')]),
        "comma-separated list of image candidate strings" => {
            write!(
                docs,
                " Expects an array of dictionaries with the keys \
                  `src` (string) and `width` (integer) or `density` (float).",
            ).unwrap();
            Type::list_without_shorthand(Type::ImageCandidate, ',')
        },
        "valid source size list" => {
            write!(
                docs,
                " Expects an array of dictionaries with the keys \
                  `condition` (string) and `size` (length).",
            ).unwrap();
            Type::list_without_shorthand(Type::SourceSize, ',')
        },
        "input type keyword" => {
            let variants = ctx
                .html
                .select(s!(
                    "table#attr-input-type-keywords > tbody > tr > td:first-child code"
                ))
                .map(|elem| elem.inner_text());
            create_str_enum(ctx, variants)
        },
        "referrer policy" => {
            let variants = ctx
                .referrer
                .select_first(s!("h2#referrer-policies ~ p"))
                .select(s!("code"))
                .map(|elem| elem.inner_text());
            let ty = create_str_enum(ctx, variants);
            Type::union_([ty, Type::NoneEmpty])
        },
        "potential destination.*" => {
            let variants = ctx
                .fetch
                .select_first(s!("p:has(#destination-type)"))
                .select(s!("code"))
                .map(|elem| elem.inner_text());
            create_str_enum(ctx, variants)
        },
        "valid navigable target name or keyword" => {
            let variants = ctx
                .html
                .select(s!("#valid-browsing-context-name-or-keyword code"))
                .map(|elem| elem.inner_text());
            Type::union_([create_str_enum(ctx, variants), Type::Str])
        },
        _ => panic!("not handled: {textual_ty} for {attr}"),
    })
}

fn rel_type(ctx: &mut Context) -> Type {
    let variants = ctx
        .html
        .select_first(s!("#table-link-relations"))
        .select(s!("tbody tr td:first-of-type code"))
        .map(|elem| elem.inner_text());
    let ty = create_str_enum(ctx, variants);
    Type::list(ty, ' ')
}

/// Tries to parse an attribute's textual as a semicolon-separate list of
/// strings.
fn try_parse_alternation(ctx: &mut Context, textual_ty: &str) -> Option<Type> {
    let mut fallback = false;

    let mut variants = vec![];
    for piece in textual_ty.split(";") {
        let piece = piece.trim();
        if piece.starts_with('"') && piece.ends_with('"') {
            variants.push(piece[1..piece.len() - 1].to_owned());
            continue;
        }
        match piece {
            "a custom command keyword" => fallback = true,
            s if s.starts_with("a valid MIME type string") => fallback = true,
            _ if !piece.is_empty() => return None,
            _ => {}
        }
    }

    let mut ty = create_str_enum(ctx, variants);
    if fallback {
        ty = Type::union_([ty, Type::Str]);
    }

    Some(ty)
}

/// Determines the Rust type for an ARIA attribute.
fn determine_aria_type(ctx: &mut Context, attr: &str) -> Type {
    let table_sel = format!("h4[id^=\"{attr}\"] ~ table[class$=\"-features\"]");
    let ty_cell = ctx
        .aria
        .select_first(&Selector::parse(&table_sel).unwrap())
        .select_first(s!("td[class$=\"-value\"]"));

    match ty_cell.inner_text().as_str() {
        "ID reference" => Type::Str,
        "ID reference list" => Type::list(Type::Str, ' '),
        "integer" => Type::Int,
        "number" => Type::Float,
        "string" => Type::Str,
        "token" => determine_aria_values(ctx, attr),
        "token list" => {
            let ty = determine_aria_values(ctx, attr);
            Type::list(ty, ' ')
        }
        "tristate" => Type::union_([
            Type::TrueFalse,
            create_str_literal(
                ctx,
                (
                    "mixed".into(),
                    "An intermediate value between true and false.".into(),
                ),
            ),
        ]),
        "true/false" => Type::TrueFalse,
        "true/false/undefined" => Type::union_([Type::TrueFalse, Type::NoneUndefined]),
        text => panic!("aria not handled: {text} for {attr}"),
    }
}

/// Determines the Rust type for an ARIA string enumeration.
fn determine_aria_values(ctx: &mut Context, attr: &str) -> Type {
    let table_sel = format!("h4[id^=\"{attr}\"] ~ table.value-descriptions");
    let variants = ctx
        .aria
        .select_first(&Selector::parse(&table_sel).unwrap())
        .select(s!("tbody tr"))
        .map(|tr| {
            (
                tr.select_text(s!(".value-name"))
                    .trim_end_matches(" (default)")
                    .to_owned(),
                tr.select_text(s!(".value-description")),
            )
        });
    create_str_enum(ctx, variants)
}

/// Allocate a string literal type in the output.
fn create_str_literal(ctx: &mut Context, variant: impl EnumVariant) -> Type {
    create_str_enum(ctx, vec![variant])
}

/// Allocates a string enum type in the output.
fn create_str_enum<V: EnumVariant>(
    ctx: &mut Context,
    variants: impl IntoIterator<Item = V>,
) -> Type {
    let mut variants: Vec<_> = variants.into_iter().map(V::with_docs).collect();
    let mut extract = |list: &[&str]| {
        let has = list
            .iter()
            .all(|item| variants.iter().any(|(v, _)| v == item));
        if has {
            variants.retain(|(v, _)| !list.contains(&v.as_str()));
        }
        has
    };

    let mut options = vec![];

    if extract(&["true", "false"]) {
        options.push(Type::TrueFalse);
    } else if extract(&["yes", "no"]) {
        options.push(Type::YesNo);
    } else if extract(&["on", "off"]) {
        options.push(Type::OnOff);
    }

    if extract(&["ltr", "rtl"]) {
        options.push(Type::HorizontalDir);
    }

    if extract(&["none"]) {
        options.push(Type::None);
    }

    if extract(&["auto"]) {
        options.push(Type::Auto);
    }

    if !variants.is_empty() {
        let len = variants.len();
        let start = (0..ctx.strings.len())
            .find(|&i| ctx.strings.get(i..i + len) == Some(variants.as_slice()))
            .unwrap_or_else(|| {
                let i = ctx.strings.len();
                ctx.strings.extend(variants);
                i
            });

        options.push(Type::Strings(start, start + len));
    }

    if options.len() == 1 {
        options.into_iter().next().unwrap()
    } else {
        Type::union_(options)
    }
}

/// A variant in a stringy enum.
trait EnumVariant {
    fn with_docs(self) -> (String, String);
}

impl EnumVariant for &str {
    fn with_docs(self) -> (String, String) {
        (self.into(), String::new())
    }
}

impl EnumVariant for String {
    fn with_docs(self) -> (String, String) {
        (self, String::new())
    }
}

impl EnumVariant for (String, String) {
    fn with_docs(self) -> (String, String) {
        self
    }
}

/// Generates the output file.
fn codegen(output: &Output) -> String {
    let elem_infos = output.element_infos.iter().map(|info| {
        let ElementInfo {
            name,
            docs,
            attributes,
        } = info;
        format!(
            "    ElemInfo::new(\n        {name:?},\n        {docs:?},\n        &[{}],\n    ),",
            attributes
                .iter()
                .map(|v| v.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    });

    let attr_infos = output.attr_infos.iter().map(|info| {
        let AttrInfo { name, ty, docs, .. } = info;
        format!(
            "    AttrInfo::new(\n        {name:?},\n        {docs:?},\n        {},\n    ),",
            ty.encode()
        )
    });

    let attr_strings = output
        .attr_strings
        .iter()
        .map(|pair| format!("    {pair:?},"));

    let mut out = String::new();
    macro_rules! w {
        ($($tts:tt)*) => {
            writeln!(out,  $($tts)*).unwrap();
        }
    }

    w!("// This file is generated by `{}`.", file!());
    w!("// Do not edit by hand.");
    w!();
    w!("#![cfg_attr(rustfmt, rustfmt_skip)]");
    w!();
    w!("use super::{{ElemInfo, AttrInfo, Type}};");
    w!();
    w!("/// A list of all HTML elements.");
    w!("pub const ELEMS: &[ElemInfo] = &[");
    w!("{}", elem_infos.join("\n"));
    w!("];");
    w!();
    w!("/// A list of all elements' attributes.");
    w!("pub const ATTRS: &[AttrInfo] = &[");
    w!("{}", attr_infos.join("\n"));
    w!("];");
    w!();
    w!("/// The first `ATTRS_GLOBAL` attributes in `ATTRS` apply to all elements.");
    w!("pub const ATTRS_GLOBAL: usize = {};", output.attrs_global);
    w!();
    w!("/// Strings referenced by [`Type::Strings`].");
    w!("pub const ATTR_STRINGS: &[(&str, &str)] = &[");
    w!("{}", attr_strings.join("\n"));
    w!("];");

    out
}

/// Postprocesses documentation.
fn docs(text: &str) -> String {
    text.replace("\n", " ")
        .replace_regex(re!("\\[[A-Z]+\\]"), "")
        .replace_regex(re!("\\s+"), " ")
        .trim()
        .trim_end_matches('.')
        .to_owned()
        + "."
}

/// Helpers methods on [`ElementRef`].
trait ElementRefExt<'a> {
    fn inner_text(&self) -> String;
    fn select_text(&self, selector: &Selector) -> String;
    fn select_first(&self, selector: &Selector) -> ElementRef<'a>;
}

impl<'a> ElementRefExt<'a> for ElementRef<'a> {
    fn inner_text(&self) -> String {
        self.text().collect()
    }

    fn select_text(&self, selector: &Selector) -> String {
        self.select(selector).flat_map(|elem| elem.text()).collect()
    }

    #[track_caller]
    fn select_first(&self, selector: &Selector) -> ElementRef<'a> {
        self.select(selector)
            .next()
            .expect("found no matching element")
    }
}

trait Join {
    fn join(self, separator: &str) -> String;
}

impl<I, T> Join for I
where
    I: Iterator<Item = T>,
    T: Display,
{
    fn join(self, separator: &str) -> String {
        self.map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(separator)
    }
}

trait StrExt {
    fn replace_regex(&self, re: &Regex, replacement: &str) -> Cow<str>;
}

impl StrExt for str {
    fn replace_regex(&self, re: &Regex, replacement: &str) -> Cow<str> {
        re.replace_all(self, replacement)
    }
}
