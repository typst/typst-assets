//! Assets for the Typst compiler.
//!
//! These are not part of the main compiler crate to keep its size down.

macro_rules! asset {
    ($path:literal) => {
        include_bytes!(concat!("../files/", $path)).as_slice()
    };
}

pub mod html;
pub mod mathml;

/// ICU data.
pub mod icu {
    /// Custom ICU segmentation data that fixes line-breaking curly quotes
    /// (`U+201C`, `U+201D`) in Chinese and Japanese. This should be removed
    /// once we can use the Unicode 17.0 line segmenter, which is coming in the
    /// next version of ICU4X.
    ///
    /// To generate the blob data, first install the patched `icu4x-datagen`
    /// binary:
    /// ```sh
    /// cargo install icu4x-datagen \
    ///   --locked \
    ///   --git https://github.com/typst/icu4x \
    ///   --branch cj-patch-update
    /// ```
    /// Use `cargo {build,run} --locked --bin icu4x-datagen --` instead when
    /// developing.
    ///
    /// Then generate the postcard blob data with the following command:
    /// ```sh
    /// icu4x-datagen \
    ///   --locales full \ # locales don't seem to affect the data
    ///   --format blob \
    ///   --overwrite \
    ///   --markers SegmenterBreakLineV1 SegmenterBreakGraphemeClusterV1 \
    ///   --out typst-assets/files/icu/icu_cj_segment.postcard
    /// ```
    /// I determined which markers to include by starting with none, then
    /// loading the postcard and reading the panic message when the constructor
    /// fails.
    pub const ICU_CJ_SEGMENT: &[u8] = asset!("icu/icu_cj_segment.postcard");
}

/// ICC profiles.
pub mod icc {
    /// The ICC profile used to convert from CMYK to RGB.
    ///
    /// This is a minimal CMYK profile that only contains the necessary
    /// information to convert from CMYK to RGB. It is based on the CGATS TR
    /// 001-1995 specification. See
    /// <https://github.com/saucecontrol/Compact-ICC-Profiles#cmyk>.
    pub const CMYK_TO_XYZ: &[u8] = asset!("icc/CMYK-to-XYZ.icc");
    pub const S_GREY_V4: &[u8] = asset!("icc/sGrey-v4.icc");
    pub const S_RGB_V4: &[u8] = asset!("icc/sRGB-v4.icc");
}

/// PDF standard fonts.
pub mod pdf {
    /// Foxit Ding Bats Font.
    pub const DING_BATS: &[u8] = asset!("fonts/FoxitDingbats.pfb");
    /// Foxit Symbol Font.
    pub const SYMBOL: &[u8] = asset!("fonts/FoxitSymbol.pfb");

    /// Foxit Fixed font.
    pub const FIXED: &[u8] = asset!("fonts/FoxitFixed.pfb");
    /// Foxit Fixed Bold font.
    pub const FIXED_BOLD: &[u8] = asset!("fonts/FoxitFixedBold.pfb");
    /// Foxit Fixed Bold Italic font.
    pub const FIXED_BOLD_ITALIC: &[u8] = asset!("fonts/FoxitFixedBoldItalic.pfb");
    /// Foxit Fixed Italic font.
    pub const FIXED_ITALIC: &[u8] = asset!("fonts/FoxitFixedItalic.pfb");

    /// Foxit Sans font.
    pub const SANS: &[u8] = asset!("fonts/FoxitSans.pfb");
    /// Foxit Sans Bold font.
    pub const SANS_BOLD: &[u8] = asset!("fonts/FoxitSansBold.pfb");
    /// Foxit Sans Bold Italic font.
    pub const SANS_BOLD_ITALIC: &[u8] = asset!("fonts/FoxitSansBoldItalic.pfb");
    /// Foxit Sans Italic font.
    pub const SANS_ITALIC: &[u8] = asset!("fonts/FoxitSansItalic.pfb");

    /// Foxit Serif font.
    pub const SERIF: &[u8] = asset!("fonts/FoxitSerif.pfb");
    /// Foxit Serif Bold font.
    pub const SERIF_BOLD: &[u8] = asset!("fonts/FoxitSerifBold.pfb");
    /// Foxit Serif Bold Italic font.
    pub const SERIF_BOLD_ITALIC: &[u8] = asset!("fonts/FoxitSerifBoldItalic.pfb");
    /// Foxit Serif Italic font.
    pub const SERIF_ITALIC: &[u8] = asset!("fonts/FoxitSerifItalic.pfb");
}

/// Bundled fonts.
///
/// This returns an empty iterator if the `fonts` feature is disabled.
pub fn fonts() -> impl Iterator<Item = &'static [u8]> {
    #[cfg(not(feature = "fonts"))]
    return [].into_iter();

    #[cfg(feature = "fonts")]
    [
        asset!("fonts/LibertinusSerif-Regular.otf"),
        asset!("fonts/LibertinusSerif-Bold.otf"),
        asset!("fonts/LibertinusSerif-Italic.otf"),
        asset!("fonts/LibertinusSerif-BoldItalic.otf"),
        asset!("fonts/LibertinusSerif-Semibold.otf"),
        asset!("fonts/LibertinusSerif-SemiboldItalic.otf"),
        asset!("fonts/NewCMMath-Bold.otf"),
        asset!("fonts/NewCMMath-Book.otf"),
        asset!("fonts/NewCMMath-Regular.otf"),
        asset!("fonts/NewCM10-Regular.otf"),
        asset!("fonts/NewCM10-Bold.otf"),
        asset!("fonts/NewCM10-Italic.otf"),
        asset!("fonts/NewCM10-BoldItalic.otf"),
        asset!("fonts/DejaVuSansMono-Bold.ttf"),
        asset!("fonts/DejaVuSansMono-BoldOblique.ttf"),
        asset!("fonts/DejaVuSansMono-Oblique.ttf"),
        asset!("fonts/DejaVuSansMono.ttf"),
    ]
    .into_iter()
}
