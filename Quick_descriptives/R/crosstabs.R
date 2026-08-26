# Crosstab / frequency helpers ------------------------------------------------
# Ported from the deprecated 'quackery' project (count_across.Rmd, ctab.Rmd,
# ctabs.Rmd, mfreq.Rmd). Bugs fixed during the port are noted per-function.

#' Frequency counts across several variables, stacked into one table
#'
#' Converts each of `vars` to a labelled factor and counts levels, stacking
#' the results into a single long-format tibble (one row per variable-level
#' combination) rather than returning a separate table per variable.
#'
#' @param data A data frame containing the variables of interest.
#' @param vars Character vector of variable names to summarise. If `NULL`
#'   (the default), every variable in `data` is used.
#' @param empty_levels Logical. If `TRUE`, unused factor levels are kept in
#'   the output. Default `FALSE`.
#' @param labels Logical. If `FALSE`, variable labels are stripped from the
#'   `var` column, leaving just the variable name. Default `TRUE`.
#' @param sort Logical. If `TRUE`, sort each variable's levels by frequency.
#'   Default `FALSE`.
#' @return A tibble with columns `var` (variable name, optionally with
#'   label), `levels`, `n`, and `p` (percentage).
#' @export
count_across <- function(data,
                          vars = NULL,
                          empty_levels = FALSE,
                          labels = TRUE,
                          sort = FALSE) {

  if (is.null(vars)) {
    vars <- names(data)
  }

  result <- purrr::map(vars, ~ data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(.x), ~ labelled::to_factor(.x, drop_unused_labels = empty_levels))) |>
    dplyr::pull(dplyr::all_of(.x)) |>
    forcats::fct_count(prop = TRUE, sort = sort) |>
    dplyr::mutate(p = round(p * 100, 2)) |>
    dplyr::mutate(
      # NOTE: original code referenced a bare `df` here instead of `data`,
      # which meant the function silently depended on a global `df` object.
      # Fixed to use the `data` argument.
      var = paste0(.x, ":- ", labelled::var_label(data |> dplyr::select(dplyr::all_of(.x)), null_action = "fill")),
      .before = 1
    ) |>
    dplyr::rename(levels = f)
  ) |>
    dplyr::bind_rows()

  if (isFALSE(labels)) {
    result <- result |> dplyr::mutate(var = stringr::str_split_i(var, ":-", 1))
  }

  return(result)
}


#' Two-way crosstab of one variable against a breakdown variable
#'
#' @param data A data frame.
#' @param vars Unquoted outcome variable to tabulate (data-masked, as in
#'   `dplyr::count()`).
#' @param by Unquoted breakdown/crossbreak variable (data-masked).
#' @param stat Which statistic to show in the crossbreak columns: `"p"`
#'   (percentage, default) or `"n"` (unweighted count).
#' @param labels Logical. If `TRUE` (default), rename the first column to
#'   `vars`'s variable label.
#' @param empty_levels Logical. If `TRUE`, keep unused factor levels.
#'   Default `FALSE`.
#' @param drop_missing Which missing data to drop before tabulating: `"none"`
#'   (default), `"b"` (both `vars` and `by`), `"p"` (predictor/`by` only), or
#'   `"o"` (outcome/`vars` only).
#' @return A tibble with one row per level of `vars`, one column per level of
#'   `by`, plus a `Total` column.
#' @export
ctab <- function(data,
                  vars = NULL,
                  by,
                  stat = "p",
                  labels = TRUE,
                  empty_levels = FALSE,
                  drop_missing = "none") {

  drop <- setdiff(c("n", "p"), stat)

  by_drop <- FALSE
  vars_drop <- FALSE
  if (drop_missing %in% c("b", "p")) by_drop <- TRUE
  if (drop_missing %in% c("b", "o")) vars_drop <- TRUE

  df_int <- data |>
    dplyr::mutate(
      dplyr::across(c({{ by }}), ~ labelled::to_factor(.x, drop_unused_labels = empty_levels, user_na_to_na = by_drop)),
      dplyr::across(c({{ vars }}), ~ labelled::to_factor(.x, drop_unused_labels = empty_levels, user_na_to_na = vars_drop))
    )

  if (drop_missing %in% c("b", "p")) df_int <- df_int |> dplyr::filter(!is.na({{ by }}))
  if (drop_missing %in% c("b", "o")) df_int <- df_int |> dplyr::filter(!is.na({{ vars }}))

  cells <- df_int |>
    dplyr::count({{ vars }}, {{ by }}) |>
    dplyr::group_by({{ by }}) |>
    dplyr::mutate(p = round(n / sum(n) * 100, 2)) |>
    dplyr::select(!dplyr::all_of(drop)) |>
    tidyr::pivot_wider(names_from = {{ by }}, values_from = dplyr::all_of(stat))

  total <- df_int |>
    dplyr::count({{ vars }}) |>
    dplyr::mutate(p = round(n / sum(n) * 100, 2)) |>
    dplyr::select(!dplyr::all_of(drop)) |>
    # NOTE: original was `rename(Total = stat)`, which renames a literal
    # column called `stat` (there isn't one) rather than the column named by
    # the *value* of `stat`. Fixed with all_of(stat) so it renames correctly.
    dplyr::rename(Total = dplyr::all_of(stat))

  suppressMessages(tbl <- dplyr::full_join(cells, total))

  if (isTRUE(labels)) {
    names(tbl)[1] <- data |> dplyr::select({{ vars }}) |> labelled::var_label(null_action = "fill", unlist = TRUE)
  }

  return(tbl)
}


#' Crosstab one or more outcomes against one or more predictors
#'
#' For each combination of `vars` (outcomes) and `preds` (predictors),
#' builds a percentage crosstab with a base row and a `total` column showing
#' the unconditional distribution of the outcome. Also always computes
#' marginal frequencies (via [count_across()]) for every variable involved
#' -- both outcomes and predictors -- as a companion overview table, since
#' you almost always want to sanity-check base rates alongside the
#' crosstabs themselves.
#'
#' @param data A data frame.
#' @param vars Character vector of outcome variable names.
#' @param preds Character vector of predictor/crossbreak variable names.
#' @return A named list with two elements: `overview` (a single tibble from
#'   [count_across()] covering every variable in `vars` and `preds`, deduped),
#'   and `tables` (a nested list: one element per `vars` entry, each
#'   containing one tibble per `preds` entry). Each table in `tables`
#'   carries a `"tags"` attribute (see [.add_tags()]) recording the
#'   predictor's variable label and the number of predictor-level columns,
#'   used by [ctabs_xlsx()] for layout.
#' @export
ctabs <- function(data, vars, preds) {
  suppressWarnings(suppressMessages({

    overview <- count_across(data, vars = unique(c(vars, preds)))

    data2 <- data |> dplyr::mutate(dplyr::across(c(vars, preds), ~ labelled::to_factor(.x)))

    tables <- purrr::pmap(list(vars), function(var) {

      int_results <- purrr::pmap(list(preds), function(pred) {

        figures <- data2 |>
          dplyr::count(.data[[pred]], .data[[var]]) |>
          dplyr::group_by(.data[[pred]]) |>
          dplyr::mutate(p = round(n / sum(n) * 100, 2))

        base <- figures |>
          dplyr::group_by(.data[[pred]]) |>
          dplyr::summarise(base = sum(n, na.rm = TRUE))

        main_table <- dplyr::bind_rows(
          figures |>
            dplyr::select(-n) |>
            tidyr::pivot_wider(names_from = .data[[pred]], values_from = p),
          base |>
            tidyr::pivot_wider(names_from = .data[[pred]], values_from = base) |>
            dplyr::mutate(!!rlang::sym(var) := "Base", .before = 1)
        )

        tot_figures <- data2 |>
          dplyr::mutate(!!rlang::sym(var) := labelled::to_factor(.data[[var]])) |>
          dplyr::count(.data[[var]]) |>
          dplyr::mutate(total = round(n / sum(n) * 100, 2))

        tot_base <- tot_figures |> dplyr::summarise(total = sum(n, na.rm = TRUE))

        total <- dplyr::bind_rows(tot_figures |> dplyr::select(total), tot_base)

        result <- dplyr::bind_cols(main_table, total)

        p_lab <- data |> dplyr::select(dplyr::all_of(pred)) |> labelled::var_label(null_action = "fill")
        p_lab_n <- ncol(result) - 2
        # NOTE: original called `add_tags(result, p_lab, p_lab_n)` without
        # reassigning the (tagged) return value back to `result`, so the tag
        # was silently discarded. Fixed below.
        result <- .add_tags(result, p_lab, p_lab_n)

        return(result)
      })

      return(int_results)
    })

    return(list(overview = overview, tables = tables))
  }))
}


#' Export ctabs() output to a formatted Excel workbook
#'
#' Always writes an "Overview" worksheet first, with the marginal
#' frequencies for every variable involved (from `input$overview`) -- this
#' isn't optional/opt-in, since you should be able to see base rates
#' alongside the crosstabs by default. Then writes one worksheet per
#' outcome variable, stacking each predictor's crosstab below the previous
#' one with a merged predictor-label header row.
#'
#' @param input The list returned by [ctabs()] (with `overview` and
#'   `tables` elements).
#' @param filename Path to the `.xlsx` file to write.
#' @return Invisibly, the path written to (via [openxlsx::saveWorkbook()]).
#' @export
ctabs_xlsx <- function(input, filename) {

  outcome_style <- openxlsx::createStyle(
    fontSize = 12,
    fontColour = "black",
    halign = "left",
    textDecoration = "bold"
  )

  header_style <- openxlsx::createStyle(
    fontSize = 10,
    fontColour = "black",
    halign = "center",
    textDecoration = "bold",
    border = "TopBottomLeftRight",
    borderColour = "grey",
    wrapText = TRUE
  )

  body_style <- openxlsx::createStyle(
    fontSize = 10,
    fontColour = "black",
    halign = "center",
    border = "TopBottomLeftRight",
    borderColour = "grey"
  )

  percentage_style <- openxlsx::createStyle(numFmt = "0\\%")

  wb <- openxlsx::createWorkbook()

  # Overview sheet -- always written first, not an optional extra.
  overview_ncol <- ncol(input$overview)
  openxlsx::addWorksheet(wb, "Overview", gridLines = FALSE)
  # na.string ensures the missing-data row that count_across() produces
  # (its `levels` column is a real NA when a variable has missing cases)
  # prints as "-" rather than a blank cell -- matches the convention already
  # used for the per-outcome crosstab tables below.
  openxlsx::writeData(wb, "Overview", x = input$overview, startCol = 1, startRow = 1, na.string = "-")
  openxlsx::addStyle(wb, sheet = "Overview", style = header_style,
                      rows = 1, cols = seq_len(overview_ncol), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = "Overview", style = body_style,
                      rows = 2:(nrow(input$overview) + 1), cols = seq_len(overview_ncol), gridExpand = TRUE)
  openxlsx::setColWidths(wb, sheet = "Overview", cols = seq_len(overview_ncol), widths = "auto")
  openxlsx::freezePane(wb, "Overview", firstActiveRow = 2, firstActiveCol = NULL)

  purrr::walk(seq_along(input$tables), function(outcome_tables) {

    cur_out_tbl <- input$tables[[outcome_tables]]
    # original used `cur_out_tbl[[1]] %>% names %>% .[1]` (magrittr dot
    # placeholder); simplified to plain base R now that the package uses the
    # native pipe, which has no equivalent placeholder for this pattern.
    outcome_name <- names(cur_out_tbl[[1]])[1]

    openxlsx::addWorksheet(wb, outcome_name, gridLines = FALSE)
    openxlsx::writeData(wb, outcome_name, x = outcome_name, startCol = 2, startRow = 2)
    openxlsx::addStyle(wb, sheet = outcome_name, style = outcome_style, rows = 2, cols = 2, gridExpand = TRUE)

    purrr::walk(seq_along(cur_out_tbl), function(table_number) {

      cur_tbl <- cur_out_tbl[[table_number]] |> dplyr::relocate(total, .after = 1)
      p_lab <- attributes(cur_tbl)$tags[[1]]
      p_lab_num <- attributes(cur_tbl)$tags[[2]]
      p_lab_merge_end <- 2 + p_lab_num
      tbl_rows <- nrow(cur_tbl)
      tbl_cols <- ncol(cur_tbl)
      starting_point <- 5
      if (table_number > 1) starting_point <- starting_point + (table_number - 1) * (4 + tbl_rows)
      header_row <- starting_point - 1
      end_row <- starting_point + tbl_rows
      end_percents <- end_row - 1
      end_col <- 2 + tbl_cols - 1

      openxlsx::writeData(wb, outcome_name, x = p_lab, startCol = 3, startRow = starting_point - 1)
      openxlsx::mergeCells(wb, sheet = outcome_name, cols = 3:p_lab_merge_end, rows = starting_point - 1)
      openxlsx::writeData(wb, outcome_name, x = cur_tbl, startCol = 2, startRow = starting_point, na.string = "-")
      openxlsx::addStyle(wb, sheet = outcome_name, style = body_style,
                          rows = starting_point:end_row, cols = 2:end_col, gridExpand = TRUE)
      openxlsx::addStyle(wb, sheet = outcome_name, style = percentage_style,
                          rows = starting_point:end_percents, cols = 2:end_col, gridExpand = TRUE, stack = TRUE)
      openxlsx::addStyle(wb, sheet = outcome_name, style = header_style,
                          rows = header_row:starting_point, cols = 2:end_col, gridExpand = TRUE)
    })

    max_end_col <- purrr::map(cur_out_tbl, ncol) |> unlist() |> max()
    openxlsx::setColWidths(wb, sheet = outcome_name, cols = 1, widths = 4)
    openxlsx::setColWidths(wb, sheet = outcome_name, cols = 2:max_end_col, widths = "auto", ignoreMergedCells = TRUE)
  })

  openxlsx::saveWorkbook(wb, file = filename, overwrite = TRUE)
}


#' Multi-variable frequency table (row or column percentages)
#'
#' Tabulates several variables that share a common response scale (e.g. a
#' battery of trust/agreement items), returning either one row per variable
#' (`long = TRUE`) or one column per variable (`long = FALSE`).
#'
#' @param data A data frame.
#' @param vars Character vector of variable names. If `NULL` (default), uses
#'   every variable in `data` (only sensible if they share response levels).
#' @param labels Logical. If `TRUE`, use variable labels instead of names.
#'   Default `FALSE`.
#' @param long Logical. If `TRUE` (default), one row per variable with
#'   levels as columns. If `FALSE`, one column per variable with levels as
#'   rows.
#' @param stat Which statistic to show: `"p"` (percentage, default) or
#'   `"n"` (count).
#' @return A tibble, shape depending on `long`, with a `Base` row/column
#'   giving the number of non-missing cases per variable.
#' @export
mfreq <- function(data,
                   vars = NULL,
                   labels = FALSE,
                   long = TRUE,
                   stat = "p") {

  stat_drop <- ifelse(stat == "p", "n", "p")
  if (is.null(vars)) vars <- names(data)

  suppressWarnings(
    tbl <- purrr::map(vars, ~ data |>
      dplyr::mutate(dplyr::across(dplyr::all_of(.x), ~ labelled::to_factor(.x))) |>
      dplyr::count(.data[[.x]]) |>
      dplyr::mutate(p = round(n / sum(n) * 100, 2)) |>
      dplyr::rename(!!rlang::sym(.x) := stat, levels = .x) |>
      dplyr::select(!dplyr::all_of(stat_drop))
    )
  )

  if (isTRUE(labels)) {
    tbl <- purrr::map2(tbl, vars, function(xxx, yyy) {
      name <- data |> dplyr::select(dplyr::all_of(yyy)) |> labelled::var_label(null_action = "fill", unlist = TRUE)
      names(xxx)[2] <- name
      return(xxx)
    })
  }

  stat_label <- ifelse(stat == "p", "Row percentages", "Row counts")

  if (isTRUE(long)) {
    result <- purrr::map(tbl, ~ .x |>
      tidyr::pivot_wider(names_from = levels, values_from = 2) |>
      dplyr::mutate(var = names(.x)[2], .before = 1)
    ) |>
      dplyr::bind_rows()
    names(result)[1] <- stat_label
  } else {
    result <- tbl |> purrr::reduce(dplyr::full_join, by = dplyr::join_by(levels))
  }

  # NOTE: original used the deprecated `summarise_all()`; modernised to
  # `summarise(across(everything(), ...))`, the current tidyverse idiom.
  bases <- data |>
    dplyr::select(dplyr::all_of(vars)) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(!is.na(.x))))

  if (isFALSE(long)) {
    result <- dplyr::bind_rows(result, bases |> dplyr::mutate(levels = "Base"))
  } else {
    bases_long <- bases |> tidyr::pivot_longer(cols = dplyr::everything(), names_to = stat_label, values_to = "Base")
    if (isTRUE(labels)) {
      labs <- data |> dplyr::select(dplyr::all_of(vars)) |> labelled::var_label(unlist = TRUE, null_action = "fill")
      bases_long <- bases_long |> dplyr::mutate(!!rlang::sym(stat_label) := labs)
    }
    result <- dplyr::full_join(result, bases_long, by = stat_label)
  }

  return(result)
}
