//! Usage: `cargo run -p typst-assets-codegen -- <job>`
//!
//! See other files for more details.

use std::io::Read;

use regex::Regex;
use scraper::{ElementRef, Html, Selector};

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

mod html;
mod mathml;

fn main() {
    match std::env::args().nth(1).as_deref() {
        Some("html") => html::main(),
        Some("mathml") => mathml::main(),
        Some(job) => panic!("unknown codegen job: {job}"),
        None => panic!("no codegen job provided"),
    }
}

fn fetch(url: &str) -> String {
    println!("Fetching {url}");
    // Can't use `Response::into_string` because it has a 10MB limit.
    let mut buf = String::new();
    ureq::get(url)
        .call()
        .unwrap()
        .into_body()
        .into_reader()
        .read_to_string(&mut buf)
        .unwrap();
    buf
}

/// Reads a spec from the directory or, if it does not exist, fetches and stores it.
fn load_spec(spec_dir: &Option<std::path::PathBuf>, url: &str) -> ElementRef<'static> {
    let text = if let Some(dir) = spec_dir {
        // Extract the last part of the URL as the filename.
        let name = url.rsplit_terminator("/").next().unwrap();
        let path = dir.join(name).with_extension("html");
        if path.exists() {
            eprintln!("Reading from {}", path.display());
            std::fs::read_to_string(&path).unwrap()
        } else {
            let text = fetch(url);
            eprintln!("Writing to {}", path.display());
            std::fs::create_dir_all(dir).unwrap();
            std::fs::write(&path, &text).unwrap();
            text
        }
    } else {
        crate::fetch(url)
    };
    Box::leak(Box::new(Html::parse_document(&text))).root_element()
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
    T: std::fmt::Display,
{
    fn join(self, separator: &str) -> String {
        self.map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(separator)
    }
}

trait StrExt {
    fn replace_regex(&self, re: &Regex, replacement: &str) -> std::borrow::Cow<'_, str>;
}

impl StrExt for str {
    fn replace_regex(&self, re: &Regex, replacement: &str) -> std::borrow::Cow<'_, str> {
        re.replace_all(self, replacement)
    }
}
