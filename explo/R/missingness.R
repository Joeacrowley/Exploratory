# Missing-data helpers ---------------------------------------------------------
# Ported from the deprecated 'quackery' project (df_miss and df_miss_brk).
# Bugs fixed during the port are noted per-function.

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

    if(ncol(data |> dplyr::select(labelled::is.labelled)) > 0){

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

  }

  return(any_na)
}


#' Missing data by variable, broken down by a grouping variable
#'
#' Reports missingness (or non-missingness) for every variable in `data`,
#' crosstabulated by the levels of `brk`, alongside a column total across all
#' cases and a count of how many levels of `brk` each variable has any valid
#' data in. Optionally formats the result as a huxtable/flextable and/or
#' writes a formatted `.xlsx` copy.
#'
#' @param data A data frame.
#' @param brk <[`data-masking`][rlang::args_data_masking]> The grouping /
#'   breakdown variable, given unquoted. Coerced to a factor with unused
#'   levels dropped.
#' @param type One of `"n_miss"` (default, number of cases missing),
#'   `"n_val"` (number not missing), `"p_miss"` (percentage missing) or
#'   `"p_val"` (percentage not missing).
#' @param show_label Logical. If `TRUE`, append each variable's `labelled`
#'   variable label to its name in the `Variable` column. Default `FALSE`.
#' @param form One of `"df"` (default, return a tibble) or `"flx"` (return a
#'   formatted flextable). When `export` is supplied a huxtable is always
#'   built; `form = "flx"` then additionally converts it to a flextable.
#' @param export Optional file path. If supplied, a formatted `.xlsx` copy is
#'   written there.
#' @return A tibble when `form = "df"` and `export` is `NULL`; a flextable
#'   when `form = "flx"`; otherwise the underlying huxtable.
#' @export
df_miss_brk <- function(data, brk, type = "n_miss", show_label = FALSE,
                        form = "df", export = NULL) {

  mis_func <- list(
    n_miss = ~ sum(is.na(.x)),
    n_val  = ~ sum(!is.na(.x)),
    p_miss = ~ round(sum(is.na(.x)) / dplyr::n() * 100, 1),
    p_val  = ~ round(sum(!is.na(.x)) / dplyr::n() * 100, 1)
  )

  type_labels <- list(
    n_miss = "number of cases missing",
    n_val  = "number of cases NOT missing",
    p_miss = "percentage of cases missing",
    p_val  = "percentage of cases NOT missing"
  )

  if (!type %in% names(mis_func)) {
    stop("`type` must be one of: ", paste(names(mis_func), collapse = ", "))
  }

  if (!form %in% c("df", "flx")) {
    stop("`form` must be one of: 'df', 'flx'")
  }

  df <- data |>
    dplyr::mutate(brk = forcats::fct_drop(forcats::as_factor({{ brk }}))) |>
    dplyr::select(-{{ brk }})

  missing_by_brk <- df |>
    dplyr::group_by(brk) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), mis_func[[type]])) |>
    tidyr::pivot_longer(cols = -brk, names_to = "Variable", values_to = "val") |>
    tidyr::pivot_wider(names_from = brk, values_from = val)

  missing_total <- df |>
    dplyr::select(-brk) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), mis_func[[type]])) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Variable", values_to = "Total (all cases)")

  result <- dplyr::full_join(missing_by_brk, missing_total, by = dplyr::join_by(Variable))

  n_cols_with_valid_cases <- df |>
    dplyr::group_by(brk) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ any(!is.na(.x)))) |>
    dplyr::summarise(dplyr::across(-brk, sum)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "Variable", values_to = "n_brk_levels_valid")

  result <- result |>
    dplyr::full_join(n_cols_with_valid_cases, by = dplyr::join_by(Variable))

  if (show_label) {
    df_minus_brk <- df |> dplyr::select(-brk)
    n_labels_in_df <- labelled::var_label(df_minus_brk, null_action = "skip") |> length()

    if (n_labels_in_df > 0) {
      result <- dplyr::full_join(
        result,
        labelled::var_label(df_minus_brk, null_action = "fill") |>
          tibble::as_tibble() |>
          tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Variable", values_to = "Label"),
        by = dplyr::join_by(Variable)
      ) |>
        dplyr::mutate(Variable = paste0(Variable, ": ", Label)) |>
        dplyr::select(-Label)
    }
  }

  # Plain tibble path -- no huxtable work needed at all
  if (form == "df" && is.null(export)) {
    return(result)
  }

  # --- huxtable build, shared by form == "flx" and export != NULL ---

  brk_label <- data |>
    dplyr::select({{ brk }}) |>
    labelled::var_label(null_action = "fill") |>
    unlist(use.names = FALSE)

  caption <- paste0("Table shows ", type_labels[[type]], " crosstabulated by ", brk_label, ".")

  ht <- result |>
    huxtable::as_hux() |>
    huxtable::set_all_padding(1) |>
    huxtable::set_outer_padding(1) |>
    huxtable::set_font_size(10) |>
    huxtable::set_font("Arial") |>
    huxtable::set_number_format(huxtable::everywhere, huxtable::everywhere, value = huxtable::fmt_pretty(digits = 2)) |>
    huxtable::set_bold(row = 1, col = huxtable::everywhere) |>
    huxtable::set_bottom_border(row = 1, col = huxtable::everywhere) |>
    huxtable::set_na_string("na") |>
    huxtable::set_outer_borders(huxtable::everywhere, huxtable::everywhere, huxtable::brdr(1, style = "solid", "black")) |>
    huxtable::set_background_color(huxtable::evens, huxtable::everywhere, "grey95") |>
    huxtable::set_align(huxtable::everywhere, huxtable::everywhere, "left") |>
    huxtable::set_wrap(huxtable::everywhere, huxtable::everywhere, TRUE) |>
    huxtable::set_width(1) |>
    huxtable::map_text_color(huxtable::by_cases(is.na(.) ~ "red")) |>
    huxtable::set_caption(caption)

  col_lengths <- apply(ht, 2, function(col) max(nchar(as.character(col))))
  relative_widths <- col_lengths / sum(col_lengths)
  huxtable::col_width(ht) <- relative_widths

  if (!is.null(export)) {

    wb <- huxtable::as_Workbook(ht = ht, sheet = "missingness")

    # Caption occupies row 1, header row 2, data from row 3 -- since the
    # caption is now always set, this offset is no longer conditional.
    end_row <- 2 + nrow(result)
    end_col <- ncol(result)

    openxlsx::removeRowHeights(wb, "missingness", rows = 1:end_row)

    openxlsx::addStyle(wb, "missingness", style = openxlsx::createStyle(wrapText = TRUE),
                       cols = 1:end_col, rows = 2:end_row, gridExpand = TRUE, stack = TRUE)

    openxlsx::removeColWidths(wb, "missingness", cols = 1:end_col)

    .col_width <- function(huxt) {
      header_row_char_length <- purrr::map_int(
        1:ncol(huxt),
        ~ tibble::as_tibble(huxt[, .x]) |> purrr::as_vector() |> stringr::str_length() |> max()
      )
      w <- header_row_char_length + 1
      w <- ifelse(w > 50, 50, w)
      w <- ifelse(w < 5, 5, w)
      w
    }

    col_widths <- .col_width(ht)
    openxlsx::setColWidths(wb, "missingness", cols = 1:end_col, widths = col_widths, ignoreMergedCells = TRUE)

    # Freeze below the caption + header rows now that the caption always exists
    openxlsx::freezePane(wb, "missingness", firstActiveRow = 3, firstActiveCol = NULL)

    openxlsx::addStyle(wb = wb, sheet = "missingness",
                       style = openxlsx::createStyle(fontSize = 14, textDecoration = "bold", valign = "center"),
                       rows = 1, cols = 1)
    openxlsx::setRowHeights(wb, "missingness", rows = 1, heights = 30)

    openxlsx::saveWorkbook(wb, file = export, overwrite = TRUE)
  }

  if (form == "flx") {
    ht <- ht |> huxtable::as_flextable() |> flextable::autofit()
  }

  ht
}
