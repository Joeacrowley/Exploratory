# Missing-data helpers ---------------------------------------------------------
# Ported from the deprecated 'quackery' project (df_miss and
# df_miss_brk_h.Rmd). Bugs fixed during the port are noted per-function.

#' Summarise missing data for every variable in a data frame
#'
#' @param data A data frame.
#' @param more Logical. If `TRUE`, also break missingness down into
#'   system-missing vs. user-defined-missing (`labelled` "user NA") counts,
#'   for labelled variables. Default `FALSE`.
#' @return A tibble with one row per variable: `n_miss`, `n_val`, `p_miss`,
#'   and (if `more = TRUE`) the same split into `na_*`/`user_*` columns.
#' @export
df_miss <- function(data, more = FALSE) {

  any_na <- data |>
    dplyr::summarise(dplyr::across(
      dplyr::everything(),
      list(split__n_miss = ~ sum(is.na(.x)), split__n_val = ~ sum(!is.na(.x))),
      .names = "{.col}_{.fn}"
    )) |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    tidyr::separate(name, into = c("variable", "stat"), sep = "_split__") |>
    tidyr::pivot_wider(names_from = stat, values_from = value) |>
    dplyr::mutate(p_miss = round(n_miss / rowSums(dplyr::pick(n_miss, n_val), na.rm = TRUE), 5), .before = n_miss)

  if (isTRUE(more)) {

    # NOTE: original referenced a bare `df` here (three times) instead of
    # `data`, so the `more = TRUE` branch silently depended on a global `df`
    # object rather than the function's own argument. Fixed to use `data`.
    NA_only <- data |>
      dplyr::select(dplyr::where(labelled::is.labelled)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ labelled::to_factor(.x, user_na_to_na = FALSE))) |>
      dplyr::summarise(dplyr::across(
        dplyr::everything(),
        list(split__na_miss = ~ sum(is.na(.x)), split__na_val = ~ sum(!is.na(.x))),
        .names = "{.col}_{.fn}"
      )) |>
      tidyr::pivot_longer(cols = dplyr::everything()) |>
      tidyr::separate(name, into = c("variable", "stat"), sep = "_split__") |>
      tidyr::pivot_wider(names_from = stat, values_from = value) |>
      dplyr::mutate(na_p_miss = round(na_miss / rowSums(dplyr::pick(na_miss, na_val), na.rm = TRUE), 5), .before = na_miss)

    user_missing <- data |>
      dplyr::select(dplyr::where(labelled::is.labelled)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::case_when(.x %in% labelled::na_values(.x) ~ 1, TRUE ~ 0))) |>
      dplyr::summarise(dplyr::across(
        dplyr::everything(),
        list(split__user_miss = ~ sum(.x == 1), split__user_val = ~ sum(.x == 0)),
        .names = "{.col}_{.fn}"
      )) |>
      tidyr::pivot_longer(cols = dplyr::everything()) |>
      tidyr::separate(name, into = c("variable", "stat"), sep = "_split__") |>
      tidyr::pivot_wider(names_from = stat, values_from = value) |>
      dplyr::mutate(user_p_miss = round(user_miss / rowSums(dplyr::pick(user_miss, user_val), na.rm = TRUE), 5), .before = user_miss)

    any_na <- purrr::reduce(list(any_na, NA_only, user_missing), dplyr::full_join, by = "variable")
  }

  return(any_na)
}


#' Missing data by variable, broken down by a grouping variable
#'
#' Reports missingness (or non-missingness) for every variable in `data`,
#' crosstabulated by the levels of `brk`, plus a flag for whether each
#' variable has any valid data at all within each level of `brk` and how
#' many levels of `brk` it has valid data in.
#'
#' @param data A data frame.
#' @param brk Character: name of the grouping/breakdown variable.
#' @param show One of `"perc_miss"` (default), `"n_miss"`, `"n_not_miss"`,
#'   `"p_not_miss"`.
#' @param keep_all_miss Logical. If `FALSE` (default), drop variables with
#'   no valid data in any level of `brk`.
#' @param sort_miss Logical. If `TRUE` (default), sort rows by how many
#'   levels of `brk` have valid data (descending).
#' @param form Unused placeholder carried over from the original
#'   implementation (kept for interface compatibility).
#' @param export Unused placeholder carried over from the original
#'   implementation (kept for interface compatibility).
#' @return A tibble tagged (see [.add_tags()]) with the `show` statistic and
#'   `brk`'s variable label, consumed by [df_miss_brk_h()].
#' @export
df_miss_brk <- function(data,
                         brk,
                         show = "perc_miss",
                         keep_all_miss = FALSE,
                         sort_miss = TRUE,
                         form = "df",
                         export = NULL) {

  any_valid <- data |>
    dplyr::group_by(.data[[brk]]) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(is.na(.x)))) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(cols = !dplyr::all_of(brk)) |>
    tidyr::pivot_wider(names_from = dplyr::all_of(brk), values_from = value) |>
    dplyr::mutate(dplyr::across(-1, ~ .x != 1, .names = "{.col}_any_val")) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      num_val = sum(dplyr::c_across(dplyr::ends_with("any_val"))),
      any_val = dplyr::case_when(num_val > 0 ~ 1, TRUE ~ 0)
    ) |>
    dplyr::select(!dplyr::ends_with("_any_val")) |>
    dplyr::mutate(dplyr::across(!dplyr::all_of(c("name", "any_val", "num_val")), ~ .x * 100)) |>
    dplyr::mutate(dplyr::across(!dplyr::all_of(c("name", "any_val", "num_val")), ~ dplyr::case_when(.x == 100 ~ NA, TRUE ~ .x)))

  if (show != "perc_miss") {

    if (show == "n_miss") {
      output <- data |>
        dplyr::group_by(.data[[brk]]) |>
        dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.x)))) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(cols = !dplyr::all_of(brk)) |>
        tidyr::pivot_wider(names_from = dplyr::all_of(brk), values_from = value)
    }

    if (show == "n_not_miss") {
      output <- data |>
        dplyr::group_by(.data[[brk]]) |>
        dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(!is.na(.x)))) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(cols = !dplyr::all_of(brk)) |>
        tidyr::pivot_wider(names_from = dplyr::all_of(brk), values_from = value) |>
        dplyr::mutate(dplyr::across(!dplyr::all_of(c("name", "any_val", "num_val")), ~ dplyr::case_when(.x == 0 ~ NA, TRUE ~ .x)))
    }

    if (show == "p_not_miss") {
      output <- data |>
        dplyr::group_by(.data[[brk]]) |>
        dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(!is.na(.x)))) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(cols = !dplyr::all_of(brk)) |>
        tidyr::pivot_wider(names_from = dplyr::all_of(brk), values_from = value) |>
        dplyr::mutate(dplyr::across(!dplyr::all_of(c("name", "any_val", "num_val")), ~ dplyr::case_when(.x == 0 ~ NA, TRUE ~ .x)))
    }

    output <- dplyr::full_join(output, any_valid |> dplyr::select(name, any_val, num_val), by = "name")

  } else {
    output <- any_valid 
  }
  
  labs <- labelled::var_label(data, null_action = "fill") |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "name", values_to = "label")

  output <- dplyr::full_join(labs, output, by = "name") |> dplyr::relocate(label, .after = name)
  output <- output %>% filter(name != brk)

  if (isTRUE(keep_all_miss)) output <- output |> dplyr::filter(any_val == 1)
  if (isTRUE(sort_miss)) output <- output |> dplyr::arrange(dplyr::desc(num_val))

  stat_type <- show
  brk_var <- data |> dplyr::select(dplyr::all_of(brk)) |> labelled::var_label(unlist = TRUE, null_action = "fill")
  # NOTE: original called `add_tags(output, stat_type, brk_var)` without
  # reassignment, so the tag never attached and df_miss_brk_h()'s caption
  # logic never fired. Fixed below.
  output <- .add_tags(output, stat_type, brk_var)

  return(output)
}


#' Format df_miss_brk() output as a huxtable/flextable, with optional Excel export
#'
#' @param data Output of [df_miss_brk()].
#' @param print `"f"` (default) to return a flextable, `"h"` to return the
#'   underlying huxtable object.
#' @param export If not `NULL`, a file path to also write a formatted
#'   `.xlsx` copy to.
#' @return A flextable or huxtable object (see `print`).
#' @export
df_miss_brk_h <- function(data, print = "f", export = NULL) {

  ht <- data |>
    huxtable::as_hux() |>
    huxtable::set_all_padding(1) |>
    huxtable::set_outer_padding(1) |>
    huxtable::set_number_format(2) |>
    huxtable::set_font_size(10) |>
    huxtable::set_font("Arial") |>
    huxtable::set_number_format(huxtable::everywhere, huxtable::everywhere, value = huxtable::fmt_pretty(digits = 2)) |>
    huxtable::set_bold(row = 1, col = huxtable::everywhere) |>
    huxtable::set_bottom_border(row = 1, col = huxtable::everywhere) |>
    huxtable::set_right_border(huxtable::everywhere, c(2, (ncol(data) - 2)), huxtable::brdr(2, "solid", "grey")) |>
    huxtable::set_na_string("na") |>
    huxtable::set_outer_borders(huxtable::everywhere, huxtable::everywhere, huxtable::brdr(1, style = "solid", "black")) |>
    huxtable::set_background_color(huxtable::evens, huxtable::everywhere, "grey95") |>
    huxtable::set_align(huxtable::everywhere, huxtable::everywhere, "left") |>
    huxtable::set_wrap(huxtable::everywhere, huxtable::everywhere, TRUE) |>
    huxtable::set_width(1) |>
    huxtable::map_text_color(huxtable::by_cases(is.na(.) ~ "red"))

  col_lengths <- apply(ht, 2, function(col) max(nchar(as.character(col))))
  relative_widths <- col_lengths / sum(col_lengths)
  huxtable::col_width(ht) <- relative_widths

  if (.has_tag(data)) {
    if (attributes(data)$tags[1] == "perc_miss") stat <- "percentage of cases missing"
    if (attributes(data)$tags[1] == "n_miss") stat <- "number of cases missing"
    if (attributes(data)$tags[1] == "n_not_miss") stat <- "number of cases NOT missing"
    if (attributes(data)$tags[1] == "p_not_miss") stat <- "percentage of cases NOT missing"
    caption <- paste0("Table shows ", stat, " crosstabulated by ", attributes(data)$tags[2], ".")
    ht <- ht |> huxtable::set_caption(caption)
  }

  if (!is.null(export)) {

    wb <- huxtable::as_Workbook(ht = ht, sheet = "missingness")
    end_row <- 2 + nrow(data)
    end_col <- ncol(data)
    openxlsx::removeRowHeights(wb, "missingness", rows = 1:end_row)

    openxlsx::addStyle(wb, "missingness", style = openxlsx::createStyle(wrapText = TRUE),
                        cols = 1:end_col, rows = 2:end_row, gridExpand = TRUE, stack = TRUE)

    openxlsx::removeColWidths(wb, "missingness", cols = 1:end_col)

    # Local helper -- column width heuristic based on formatted content length.
    .col_width <- function(huxt) {
      header_row_char_length <- purrr::map_int(
        1:ncol(huxt),
        ~ tibble::as_tibble(huxt[, .x]) |> purrr::as_vector() |> stringr::str_length() |> max()
      )
      header_proposed_width <- header_row_char_length + 1
      header_proposed_width_cut_off <- ifelse(header_proposed_width > 50, 50, header_proposed_width)
      header_proposed_width_cut_off <- ifelse(header_proposed_width_cut_off < 5, 5, header_proposed_width_cut_off)
      return(header_proposed_width_cut_off)
    }

    col_widths <- .col_width(ht)
    openxlsx::setColWidths(wb, "missingness", cols = 1:end_col, widths = col_widths, ignoreMergedCells = TRUE)
    openxlsx::freezePane(wb, "missingness", firstActiveRow = 2, firstActiveCol = NULL)

    if (.has_tag(data)) {
      openxlsx::addStyle(wb = wb, sheet = "missingness",
                          style = openxlsx::createStyle(fontSize = 14, textDecoration = "bold", valign = "center"),
                          rows = 1, cols = 1)
      openxlsx::setRowHeights(wb, "missingness", rows = 1, heights = 30)
    }

    openxlsx::saveWorkbook(wb, file = export, overwrite = TRUE)
  }

  if (print == "f") ht <- ht |> huxtable::as_flextable() |> flextable::autofit()

  return(ht)
}
