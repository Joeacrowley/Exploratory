# Predictor-level breakdown helpers ---------------------------------------------
# Ported from the deprecated 'quackery' project (return_one.Rmd). Only the
# "2" versions were brought across -- return_one()/return_one_format() were
# dropped as superseded by return_one2()/return_one2_format() per the
# pilot's scope (they share the same underlying logic; return_one2() adds
# the ability to combine several statistics -- n_val/p_val/p_na/recode/mean
# -- into one table instead of returning just one at a time).

#' One outcome summarised by one or more predictors, several statistics at once
#'
#' Cross-tabulates a single outcome variable (`out`) by one or more
#' predictors (`preds`), stacking together any combination of: number of
#' valid cases (`lev = "n_val"`), percentage valid (`"p_val"`), percentage
#' missing (`"p_na"`), percentage of cases matching a recoded set of levels
#' (`recode`), and/or the mean of `out` (`mean = TRUE`).
#'
#' @param data A data frame.
#' @param out Character: name of the outcome variable.
#' @param lev Character vector, any of `"n_val"`, `"p_val"`, `"p_na"`, to
#'   include those missingness statistics as extra rows.
#' @param recode Character vector of levels of `out` to collapse into a
#'   binary "in this group" indicator, reported as a percentage.
#' @param mean Logical. If `TRUE`, also report the numeric mean of `out`
#'   (after stripping labels). Default `FALSE`.
#' @param preds Character vector of predictor variable names.
#' @param pred_cols Which of `preds` to place in columns rather than rows:
#'   `NULL` (default, uses `preds[1]`), a numeric index into `preds`, or a
#'   character vector of predictor names.
#' @param user_na Logical. If `FALSE`, `labelled` user-defined missing
#'   values in `preds` are kept as valid categories rather than converted to
#'   `NA`. Default `TRUE`.
#' @param filter_na Which missing data to drop before summarising: `"b"`
#'   (both `out` and `preds`, default), `"o"` (outcome only), `"p"`
#'   (predictors only), or `"n"` (don't filter).
#' @return A named list with `"Outcome label"`, `"Predictors in columns"`,
#'   `"Table"` (the summary tibble), and `"Names of preds in rows"`, in the
#'   form expected by [return_one2_format()].
#' @export
return_one2 <- function(data,
                         out,
                         lev = NULL,
                         recode = NULL,
                         mean = FALSE,
                         preds,
                         pred_cols = NULL,
                         user_na = TRUE,
                         filter_na = "b") {

  all_vars <- c(out, preds)

  pred_cols_selected <- preds[1]
  if (is.numeric(pred_cols)) pred_cols_selected <- preds[pred_cols]
  if (is.character(pred_cols)) pred_cols_selected <- pred_cols
  preds_in_rows <- setdiff(preds, pred_cols_selected)

  data_int <- data |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(preds), ~ labelled::to_factor(.x, user_na_to_na = user_na)),
      dplyr::across(dplyr::all_of(out), ~ labelled::user_na_to_na(.x))
    )

  filt_vars <- all_vars
  if (filter_na == "o") filt_vars <- out
  if (filter_na == "p") filt_vars <- preds
  if (filter_na != "n") {
    data_int <- data_int |> dplyr::filter(dplyr::if_all(dplyr::all_of(filt_vars), ~ !is.na(.)))
  }

  result <- list()

  if (!is.null(lev)) {

    if (lev[1] %in% c("n_val", "p_na", "p_val")) {
      data_int_for_cats <- data_int |>
        dplyr::mutate(out2 = dplyr::case_when(is.na(.data[[out]]) ~ 1, TRUE ~ 0))
    }

    if ("n_val" %in% lev) {
      n_val <- data_int_for_cats |>
        dplyr::group_by(dplyr::across(dplyr::all_of(preds))) |>
        dplyr::summarise(result = sum(out2 == 0, na.rm = TRUE)) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = dplyr::all_of(pred_cols_selected), values_from = result) |>
        dplyr::mutate(stat = "n_val", .after = length(preds_in_rows)) |>
        dplyr::mutate(sort = 3)
      result <- append(result, list(n_val))
    }

    if ("p_val" %in% lev) {
      p_val <- data_int_for_cats |>
        dplyr::group_by(dplyr::across(dplyr::all_of(preds))) |>
        dplyr::summarise(result = round(mean(out2 == 0, na.rm = TRUE) * 100, 2)) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = dplyr::all_of(pred_cols_selected), values_from = result) |>
        dplyr::mutate(stat = "p_val", .after = length(preds_in_rows)) |>
        dplyr::mutate(sort = 4)
      result <- append(result, list(p_val))
    }

    if ("p_na" %in% lev) {
      p_na <- data_int_for_cats |>
        dplyr::group_by(dplyr::across(dplyr::all_of(preds))) |>
        dplyr::summarise(result = round(mean(out2 == 1, na.rm = TRUE) * 100, 2)) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = dplyr::all_of(pred_cols_selected), values_from = result) |>
        dplyr::mutate(stat = "p_na", .after = length(preds_in_rows)) |>
        dplyr::mutate(sort = 5)
      result <- append(result, list(p_na))
    }
  }

  if (!is.null(recode)) {

    data_int_for_cats <- data_int |>
      dplyr::mutate(dplyr::across(dplyr::all_of(out), ~ labelled::to_factor(.x))) |>
      dplyr::mutate(out2 = dplyr::case_when(.data[[out]] %in% recode ~ 1, is.na(.data[[out]]) ~ NA, TRUE ~ 0))

    collapse_recode <- paste0("% in ", paste0(recode, collapse = ", "))
    recoded <- data_int_for_cats |>
      dplyr::group_by(dplyr::across(dplyr::all_of(preds))) |>
      dplyr::summarise(result = round(mean(out2 == 1, na.rm = TRUE) * 100, 2)) |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(names_from = dplyr::all_of(pred_cols_selected), values_from = result) |>
      dplyr::mutate(stat = collapse_recode, .after = length(preds_in_rows)) |>
      dplyr::mutate(sort = 1)
    result <- append(result, list(recoded))
  }

  if (isTRUE(mean)) {

    data_int_for_num <- data_int |>
      # NOTE: zap_labels() is a haven function, not labelled:: -- fixed after
      # R CMD check flagged `labelled::zap_labels` as missing/unexported.
      dplyr::mutate(out2 = haven::zap_labels(as.numeric(labelled::user_na_to_na(.data[[out]]))))

    # NOTE: renamed the local variable from `mean` (as in the original) to
    # `mean_tbl` -- the *argument* is already called `mean`, and reusing
    # the same name for a local object one line later works (R resolves
    # `mean(...)` used in call position to the function, not the logical
    # argument) but reads confusingly. Behaviour is unchanged.
    mean_tbl <- data_int_for_num |>
      dplyr::group_by(dplyr::across(dplyr::all_of(preds))) |>
      dplyr::summarise(result = round(mean(out2, na.rm = TRUE), 4)) |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(names_from = dplyr::all_of(pred_cols_selected), values_from = result) |>
      dplyr::mutate(stat = "mean", .after = length(preds_in_rows)) |>
      dplyr::mutate(sort = 2)

    result <- append(result, list(mean_tbl))
  }

  output <- purrr::reduce(result, dplyr::bind_rows) |> dplyr::arrange(sort) |> dplyr::select(-sort)

  pred_col_var_labels <- data |>
    dplyr::select(dplyr::all_of(pred_cols_selected)) |>
    labelled::var_label(unlist = TRUE, null_action = "fill")

  out_var_label <- data |>
    dplyr::select(dplyr::all_of(out)) |>
    labelled::var_label(unlist = TRUE, null_action = "fill")

  outputs <- list(out_var_label, pred_col_var_labels, output, preds_in_rows)
  names(outputs) <- c("Outcome label", "Predictors in columns", "Table", "Names of preds in rows")

  return(outputs)
}


#' Format return_one2() output as a huxtable/flextable
#'
#' @param input Output of [return_one2()].
#' @param print `"h"` (default) to return a huxtable object, `"f"` to
#'   return a flextable.
#' @param colour Logical. If `TRUE`, colour-scale the data cells (excluding
#'   the caption and header rows) by value. Default `FALSE`.
#' @return A huxtable or flextable object (see `print`).
#' @export
return_one2_format <- function(input, print = "h", colour = FALSE) {

  data <- input$Table
  vals <- input$Table |> unlist() |> as.numeric() |> range(na.rm = TRUE)
  col_num <- ncol(input$Table)
  n_pred_rows <- length(input$`Names of preds in rows`)
  data_start <- n_pred_rows + 1
  pred_labs <- paste0(input$`Predictors in columns`, collapse = " X ")
  caption <- paste0(input$`Outcome label`, ". Column variables: ", pred_labs)

  hux0 <- data |> huxtable::as_hux()

  ht <- hux0 |>
    huxtable::insert_row(rep(caption, ncol(hux0))) |>
    huxtable::merge_across(row = 1) |>
    huxtable::set_all_padding(1) |>
    huxtable::set_outer_padding(1) |>
    huxtable::set_number_format(2) |>
    huxtable::set_font_size(10) |>
    huxtable::set_font("Arial") |>
    huxtable::set_number_format(huxtable::everywhere, huxtable::everywhere, value = huxtable::fmt_pretty(digits = 2)) |>
    huxtable::set_bold(row = 1:2, col = huxtable::everywhere) |>
    huxtable::set_all_borders(huxtable::everywhere, huxtable::everywhere, huxtable::brdr(1, "solid", "grey")) |>
    huxtable::set_bottom_border(row = 2, col = huxtable::everywhere, huxtable::brdr(1, "solid", "black")) |>
    huxtable::set_right_border(huxtable::everywhere, c(n_pred_rows, n_pred_rows + 1), huxtable::brdr(2, "solid", "grey")) |>
    huxtable::set_na_string("-") |>
    huxtable::set_outer_borders(huxtable::everywhere, huxtable::everywhere, huxtable::brdr(1, style = "solid", "black")) |>
    huxtable::set_background_color(huxtable::evens, huxtable::everywhere, "grey95") |>
    huxtable::set_background_color(1:2, huxtable::everywhere, "grey90") |>
    huxtable::set_align(huxtable::everywhere, huxtable::everywhere, "center") |>
    huxtable::set_align(huxtable::everywhere, 1:n_pred_rows, "center") |>
    huxtable::set_wrap(huxtable::everywhere, huxtable::everywhere, TRUE) |>
    huxtable::set_width(1)

  # NOTE: in the source Rmd this block was commented out in
  # return_one2_format() (present and working in the return_one_format()
  # sibling that this pilot dropped as superseded). Restored here since it's
  # clearly intended behaviour, not a deliberate omission -- remove this
  # block if you'd rather keep colour = FALSE as a permanent no-op.
  if (isTRUE(colour)) {
    ht <- ht |>
      huxtable::map_background_color(
        row = -c(1, 2), col = data_start:col_num,
        huxtable::by_colorspace(c("#f0deed", "#b053a1"), range = vals, na_color = "grey93")
      )
  }

  if (print == "f") ht <- ht |> huxtable::as_flextable() |> flextable::autofit()

  return(ht)
}
