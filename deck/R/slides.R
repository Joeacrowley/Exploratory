# Slide builders for the Sage Mint template.
#
# All of them:
#   * take the deck as the first argument and return it invisibly, so they
#     work both in a pipe (`ppt |> add_title_slide(...)`) and as a bare
#     statement (officer mutates the rpptx environment in place);
#   * accept character vectors for text placeholders - multiple elements
#     render on separate lines;
#   * target placeholders by their label (see `mint_layouts()`).

#' Add a title slide
#'
#' @param slides An `rpptx` object ([officer::read_pptx()]).
#' @param title Character vector for the title placeholder.
#' @param subtitle Optional character vector for the subtitle.
#'
#' @return The modified `slides` object, invisibly.
#' @export
#' @examples
#' mint_pptx() |> add_title_slide("Quarterly review", subtitle = "Q3 2026")
add_title_slide <- function(slides, title, subtitle = NULL) {
  .mint_require_master(slides)
  slides <- .mint_new_slide(slides, "title", title,
                            title_label = "Text Placeholder 9")
  slides <- .mint_maybe(slides, subtitle, "Subtitle 2")
  invisible(slides)
}

#' Add a section divider slide
#'
#' @param slides An `rpptx` object ([officer::read_pptx()]).
#' @param text Character vector for the divider's main text.
#' @param footer Optional character vector. Note the footer placeholder on
#'   this layout sits low and is only ~0.4in tall, so long / multi-line
#'   footers overflow the slide.
#' @param header Optional character vector shown above the main text.
#'
#' @return The modified `slides` object, invisibly.
#' @export
#' @examples
#' mint_pptx() |> add_divider_slide("Part two", header = "Section")
add_divider_slide <- function(slides, text, footer = NULL, header = NULL) {
  .mint_require_master(slides)
  slides <- .mint_new_slide(slides, "divider", text,
                            title_label = "Text Placeholder 9")
  slides <- .mint_maybe(slides, footer, "Straight Connector 3")
  slides <- .mint_maybe(slides, header, "Subtitle 2")
  invisible(slides)
}

#' Add a closing / end-message slide
#'
#' @param slides An `rpptx` object ([officer::read_pptx()]).
#' @param text Character vector for the main message.
#' @param subtitle Optional character vector shown below it.
#'
#' @return The modified `slides` object, invisibly.
#' @export
#' @examples
#' mint_pptx() |> add_end_slide("Thank you", subtitle = "questions?")
add_end_slide <- function(slides, text, subtitle = NULL) {
  .mint_require_master(slides)
  slides <- .mint_new_slide(slides, "end", text,
                            title_label = "Text Placeholder 9")
  slides <- .mint_maybe(slides, subtitle, "Subtitle 2")
  invisible(slides)
}

#' Add a full-width text content slide
#'
#' @param slides An `rpptx` object ([officer::read_pptx()]).
#' @param title Character vector for the title.
#' @param text Character vector for the body. Multiple elements become
#'   separate bullet lines.
#' @param footer Optional character vector.
#'
#' @return The modified `slides` object, invisibly.
#' @export
#' @examples
#' mint_pptx() |> full_width_content_slide("Findings", c("Point one", "Point two"))
full_width_content_slide <- function(slides, title, text, footer = NULL) {
  .mint_require_master(slides)
  slides <- .mint_new_slide(slides, "full_width_content", title)
  slides <- officer::ph_with(
    slides, text,
    officer::ph_location_label(ph_label = "Content Placeholder 2")
  )
  slides <- .mint_maybe(slides, footer, "Footer")
  invisible(slides)
}

#' Add a two-column text slide
#'
#' @param slides An `rpptx` object ([officer::read_pptx()]).
#' @param title Character vector for the title.
#' @param left_text,right_text Optional character vectors for the columns.
#' @param footer Optional character vector.
#'
#' @return The modified `slides` object, invisibly.
#' @export
#' @examples
#' mint_pptx() |>
#'   double_text_column_slide("Trade-offs", left_text = "Pros", right_text = "Cons")
double_text_column_slide <- function(slides, title, left_text = NULL,
                                     right_text = NULL, footer = NULL) {
  .mint_require_master(slides)
  slides <- .mint_new_slide(slides, "two_column", title)
  slides <- .mint_maybe(slides, left_text, "Left Content")
  slides <- .mint_maybe(slides, right_text, "Right Content")
  slides <- .mint_maybe(slides, footer, "Footer")
  invisible(slides)
}

#' Add a full-width table slide
#'
#' @param slides An `rpptx` object ([officer::read_pptx()]).
#' @param title Character vector for the title.
#' @param text Character vector for the description above the table.
#' @param table A [flextable::flextable()], e.g. from [make_flex()].
#' @param footer Optional character vector.
#' @param control_height If `TRUE` (default), shrink the table to the
#'   template's table area with [fit_to_height()] then
#'   [flextable::fit_to_width()].
#' @param max_height,max_width Bounding box for the table when
#'   `control_height = TRUE`; height in inches, width in centimetres.
#'   Defaults are the template's table-area size.
#'
#' @return The modified `slides` object, invisibly.
#' @export
#' @examples
#' mint_pptx() |>
#'   full_width_table_slide("Data", "First rows", make_flex(head(mtcars, 5)))
full_width_table_slide <- function(slides, title, text, table, footer = NULL,
                                   control_height = TRUE,
                                   max_height = .mint_table_max_height_in,
                                   max_width = .mint_table_max_width_cm) {
  .mint_require_master(slides)
  if (isTRUE(control_height)) {
    table <- fit_to_height(table, max_height = max_height)
    table <- flextable::fit_to_width(table, max_width = max_width, unit = "cm")
  }
  slides <- .mint_new_slide(slides, "full_width_table", title)
  slides <- officer::ph_with(
    slides, text,
    officer::ph_location_label(ph_label = "Description")
  )
  slides <- officer::ph_with(
    slides, table,
    officer::ph_location_label(ph_label = "Table Area")
  )
  slides <- .mint_maybe(slides, footer, "Footer")
  invisible(slides)
}

#' Add a full-width chart slide
#'
#' @param slides An `rpptx` object ([officer::read_pptx()]).
#' @param title Character vector for the title.
#' @param text Optional character vector for the description above the chart.
#' @param chart A chart object, e.g. from [mschart::ms_barchart()].
#' @param source Optional character vector for the source line.
#' @param footer Optional character vector.
#'
#' @return The modified `slides` object, invisibly.
#' @export
full_width_chart_slide <- function(slides, title, text = NULL, chart,
                                   source = NULL, footer = NULL) {
  .mint_require_master(slides)
  slides <- .mint_new_slide(slides, "full_width_chart", title)
  slides <- officer::ph_with(
    slides, chart,
    officer::ph_location_label(ph_label = "Chart Area")
  )
  slides <- .mint_maybe(slides, text, "Description")
  slides <- .mint_maybe(slides, source, "Source")
  slides <- .mint_maybe(slides, footer, "Footer")
  invisible(slides)
}

#' Add a slide with a text column and a chart column
#'
#' @param slides An `rpptx` object ([officer::read_pptx()]).
#' @param title Character vector for the title.
#' @param text Character vector for the text column.
#' @param chart A chart object, e.g. from [mschart::ms_barchart()].
#' @param source Optional character vector for the source line.
#' @param footer Optional character vector.
#'
#' @return The modified `slides` object, invisibly.
#' @export
text_and_chart_slide <- function(slides, title, text, chart,
                                 source = NULL, footer = NULL) {
  .mint_require_master(slides)
  slides <- .mint_new_slide(slides, "text_and_chart", title)
  slides <- officer::ph_with(
    slides, text,
    officer::ph_location_label(ph_label = "Text Area")
  )
  slides <- officer::ph_with(
    slides, chart,
    officer::ph_location_label(ph_label = "Chart Area")
  )
  slides <- .mint_maybe(slides, source, "Source")
  slides <- .mint_maybe(slides, footer, "Footer")
  invisible(slides)
}
