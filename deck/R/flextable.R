#' Turn a data frame into a Sage Mint styled flextable
#'
#' Zebra striping, thin interior rules, black lines under the header and at
#' the foot of the body, and double formatting to a fixed number of digits.
#' Apply further [flextable][flextable::flextable] edits after this.
#'
#' @param df A data frame.
#' @param caption Optional caption string. `NULL` (or `FALSE`) for no
#'   caption; a string adds an auto-numbered Word caption with style
#'   `"Table: Heading row_"`.
#' @param digits Decimal places for double columns.
#'
#' @return A [flextable::flextable()].
#' @export
#' @examples
#' make_flex(head(mtcars, 5), caption = "First five rows", digits = 1)
make_flex <- function(df, caption = NULL, digits = 0) {
  ft <- flextable::flextable(df)
  ft <- flextable::set_table_properties(ft, layout = "autofit", width = 1)
  ft <- flextable::theme_zebra(
    ft,
    odd_header = "grey90",
    odd_body   = "white",
    even_body  = "#f3f8fc"
  )
  ft <- flextable::border(
    ft,
    part   = "body",
    border = officer::fp_border(color = "grey80", width = 1)
  )
  ft <- flextable::hline(
    ft,
    i = 1, part = "header",
    border = officer::fp_border(color = "black", width = 1)
  )
  ft <- flextable::hline_bottom(
    ft,
    part = "body",
    border = officer::fp_border(color = "black", width = 1)
  )
  ft <- flextable::colformat_double(ft, digits = digits)

  if (!is.null(caption) && !isFALSE(caption)) {
    ft <- flextable::set_caption(
      ft,
      caption = caption,
      autonum = officer::run_autonum(seq_id = "tab"),
      word_stylename = "Table: Heading row_"
    )
  }
  ft
}

#' Shrink a flextable to fit a maximum height
#'
#' A heuristic: if the table's natural height exceeds `max_height` it first
#' trims cell padding, and if that is not enough it sets every row to a
#' uniform height. Row heights are advisory in PowerPoint, so this reduces
#' overflow rather than guaranteeing a hard cap. Measurements are in inches
#' (matching [flextable::flextable_dim()]).
#'
#' @param ft A [flextable::flextable()].
#' @param max_height Maximum height in inches.
#' @param add_header Count the header rows when dividing the height.
#'
#' @return A [flextable::flextable()].
#' @export
#' @examples
#' fit_to_height(make_flex(mtcars), max_height = 3.3)
fit_to_height <- function(ft, max_height, add_header = TRUE) {
  natural_height <- flextable::flextable_dim(ft)$height

  if (natural_height > max_height) {
    ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
    natural_height <- flextable::flextable_dim(ft)$height
  }

  if (natural_height > max_height) {
    n_rows <- flextable::nrow_part(ft, part = "body")
    if (add_header) {
      n_rows <- n_rows + flextable::nrow_part(ft, part = "header")
    }
    ft <- flextable::height_all(ft, height = max_height / n_rows)
  }

  ft
}
