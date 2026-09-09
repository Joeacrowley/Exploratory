# Internal specification of the bundled "Sage Mint Theme" template.
# Everything template-specific (master name, layout names, placeholder
# labels, table-area geometry) lives here so a template change is a
# one-file edit.

# Master name, as it appears in officer::layout_properties()$master_name.
.mint_master <- "Sage Mint Theme"

# Layout names keyed by a short internal slug.
.mint_layout <- c(
  title              = "Title: Green",
  divider            = "Divider: Green",
  end                = "End message: Green",
  full_width_content = "Title and full width content",
  two_column         = "Title and two column content",
  full_width_table   = "Title and full width table with description",
  full_width_chart   = "Title and full width chart with description",
  text_and_chart     = "Text and chart with source"
)

# Usable area of the "Table Area" placeholder on the full-width-table
# layout, used as the default bounding box in full_width_table_slide().
# These are the author's original hand-measured values; flextable wants
# height in inches and width in centimetres.
.mint_table_max_height_in <- 8.42 / 2.54
.mint_table_max_width_cm  <- 30.86

# Shared guard: fail early with a useful message rather than letting
# officer error deep inside ph_with().
.mint_require_master <- function(slides) {
  if (!inherits(slides, "rpptx")) {
    stop("`slides` must be an rpptx object; start from mint_pptx().",
         call. = FALSE)
  }
  masters <- unique(officer::layout_properties(slides)$master_name)
  if (!.mint_master %in% masters) {
    stop(sprintf(
      "Master '%s' not found in this deck. Start from mint_pptx().",
      .mint_master
    ), call. = FALSE)
  }
  invisible(slides)
}

# add_slide() + ph_with(title) is the opening move of every builder.
.mint_new_slide <- function(slides, layout_slug, title = NULL,
                            title_label = "Title 1") {
  slides <- officer::add_slide(
    slides,
    layout = .mint_layout[[layout_slug]],
    master = .mint_master
  )
  if (!is.null(title)) {
    slides <- officer::ph_with(
      slides, title,
      officer::ph_location_label(ph_label = title_label)
    )
  }
  slides
}

# ph_with() only when the value is supplied.
.mint_maybe <- function(slides, value, ph_label) {
  if (is.null(value)) {
    return(slides)
  }
  officer::ph_with(slides, value, officer::ph_location_label(ph_label = ph_label))
}
