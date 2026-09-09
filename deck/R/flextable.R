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
#' trims cell padding, and if that is not enough it forces every row to a
#' uniform height. Row heights are advisory in PowerPoint and font size is
#' left untouched, so this reduces overflow rather than guaranteeing a hard
#' cap. When the padding trim is not enough and rows have to be compressed,
#' the table will still overflow in PowerPoint and a warning is raised
#' (unless `warn = FALSE`); the fix then is editorial - fewer rows,
#' fewer/narrower columns, a smaller font, or splitting across slides.
#' Measurements are in inches (matching [flextable::flextable_dim()]).
#'
#' @param ft A [flextable::flextable()].
#' @param max_height Maximum height in inches.
#' @param add_header Count the header rows when dividing the height.
#' @param warn Warn when rows had to be compressed below their natural
#'   height (i.e. the table will still overflow the slide).
#'
#' @return A [flextable::flextable()].
#' @export
#' @examples
#' fit_to_height(make_flex(mtcars), max_height = 3.3)
fit_to_height <- function(ft, max_height, add_header = TRUE, warn = TRUE) {
  natural_height <- flextable::flextable_dim(ft)$height

  if (natural_height > max_height) {
    ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
    natural_height <- flextable::flextable_dim(ft)$height
  }

  # Still over after trimming padding: forcing uniform row heights below what
  # the content wants. flextable_dim() would now report the forced height, so
  # it can't tell us whether the result really fits - judge from the natural
  # (pre-force) height instead.
  if (natural_height > max_height) {
    n_rows <- flextable::nrow_part(ft, part = "body")
    if (add_header) {
      n_rows <- n_rows + flextable::nrow_part(ft, part = "header")
    }
    ft <- flextable::height_all(ft, height = max_height / n_rows)

    if (warn) {
      warning(sprintf(
        paste0("Table wants %.2f in but max_height is %.2f in. Rows were ",
               "compressed to fit the grid; text will still overflow in ",
               "PowerPoint. Reduce rows/columns or the font size."),
        natural_height, max_height
      ), call. = FALSE)
    }
  }

  ft
}
