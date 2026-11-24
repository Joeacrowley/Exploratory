#' A function for some pre-set huxtable preferences
#' 
#' Applies huxtable formatting to a tibble or data frame and allows several 
#' standard options to be tweaked. Prints as a flextable when called but is
#' actually a huxtable underneath. 
#' 
#' @param tbl A data frame
#' @param na_val A string shown in place of `NA` in cells. 
#' @param merge_repeats `FALSE` by default, or the number of the columns to be
#'  merged where they repeat across rows. Can also accept huxtable arguments 
#'  like `everywhere`. 
#' @param zebra `FALSE` where not false shades alternate rows.
#' @param footnote `FALSE` a vector of character strings, each one on each line. 
#' @param caption `FALSE` String to appear as title. 
#' @param grid `FALSE` When not FALSE cell borders appear.
#' @param type_size `FALSE` Font size.
#' @param head_size `FALSE` Heading font size, over writes `type_size`.
#' @param labelled_to_factor `TRUE` Converts labelled columns to factors 
#'  prefixed with numbers. 
#' @return A `huxy` / `huxtable` object. 
#' @export
huxy <- function(
    tbl,
    na_val = "",
    merge_repeats = FALSE,
    zebra = FALSE,
    footnote = FALSE,
    caption = FALSE,
    grid = FALSE,
    type_size = FALSE,
    head_size = FALSE,
    labelled_to_factor = TRUE
    ) {

  if (isTRUE(labelled_to_factor)) {
    tbl <- tbl |>
      dplyr::mutate(dplyr::across(
        dplyr::where(labelled::is.labelled),
        ~ labelled::to_factor(.x, levels = "prefixed")
      ))
  }

  row_n <- nrow(tbl) + 1

  tbl2 <- tbl |> huxtable::as_huxtable()

  if (!isFALSE(type_size)) {
    tbl2 <- tbl2 |> huxtable::set_font_size(type_size)
  }

  if (!isFALSE(head_size)) {
    tbl2 <- tbl2 |> huxtable::set_font_size(row = 1, col = huxtable::everywhere, head_size)
  }

  if (isTRUE(grid)) {
    tbl2 <- tbl2 |> huxtable::set_all_borders(
      row = huxtable::everywhere, col = huxtable::everywhere,
      value = huxtable::brdr(0.4, "solid", "grey80")
    )
  }

  if (!isFALSE(caption)) {
    tbl2 <- tbl2 |>
      huxtable::set_caption(caption) |>
      huxtable::set_caption_pos("topleft")
  }

  if (isTRUE(zebra)) {
    tbl2 <- tbl2 |> huxtable::map_background_color(
      row = -1, col = huxtable::everywhere,
      huxtable::by_rows("#F3F7FA", "grey98")
    )
  }

  if (!isFALSE(footnote)) {
    for (note in seq_along(footnote)) {
      tbl2 <- tbl2 |> huxtable::add_footnote(footnote[note], border = NULL)
    }
  }

  if (!isFALSE(merge_repeats)) {
    tbl2 <- tbl2 |> huxtable::merge_repeated_rows(
      row = huxtable::everywhere, col = merge_repeats
    )
  }

  # Common format...
  tbl2 <- tbl2 |>
    huxtable::set_na_string(na_val) |>
    huxtable::set_bold(row = 1, col = huxtable::everywhere, value = TRUE) |>
    huxtable::set_bottom_border(row = c(1, row_n), col = huxtable::everywhere,
                                value = huxtable::brdr(0.4, "solid", "black")) |>
    huxtable::set_top_border(row = 1, col = huxtable::everywhere,
                             value = huxtable::brdr(0.4, "solid", "black"))

  # Still a huxtable, but with a class that controls how it displays
  class(tbl2) <- c("huxy", class(tbl2))
  return(tbl2)
}


#' @rdname huxy
#' @param x A `huxy` object.
#' @param ... Passed on to the flextable print / knit_print method.
#' @export
print.huxy <- function(x, ...) {
  print(huxtable::as_flextable(x), ...)
  invisible(x)
}

#' @rdname huxy
#' @exportS3Method knitr::knit_print
knit_print.huxy <- function(x, ...) {
  knitr::knit_print(huxtable::as_flextable(x), ...)
}
