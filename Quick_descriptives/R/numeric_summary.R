# Numeric summary helpers -------------------------------------------------------
# Ported from the deprecated 'quackery' project (num_crs.Rmd, num_sum.Rmd).
# Bugs fixed during the port are noted per-function.

#' Numeric summary statistics, broken down by a grouping variable
#'
#' @param data A data frame.
#' @param nums Character vector of numeric variable names to summarise.
#' @param brk Character: name of the grouping/breakdown variable.
#' @param stats Character vector of statistics to keep: any of `"mean"`,
#'   `"median"`, `"sd"`, `"min"`, `"max"`, `"n_missing"`, `"n_valid"`. Default
#'   `"all"` keeps all of them.
#' @param order `"stat"` (default) to sort by variable order as given, or
#'   `"var"` to sort alphabetically by variable then stat.
#' @return A tibble with one row per variable/statistic combination, one
#'   column per level of `brk`, plus a `Total` column.
#' @export
num_crs <- function(data,
                     nums,
                     brk,
                     stats = "all",
                     order = "stat") {

  stat_fns <- list(
    split__mean = ~ mean(.x, na.rm = TRUE),
    split__median = ~ stats::median(.x, na.rm = TRUE),
    split__sd = ~ stats::sd(.x, na.rm = TRUE),
    split__min = ~ min(.x, na.rm = TRUE),
    split__max = ~ max(.x, na.rm = TRUE),
    split__n_missing = ~ sum(is.na(.x)),
    split__n_valid = ~ sum(!is.na(.x))
  )

  data_int <- data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(brk), ~ labelled::to_factor(.x))) |>
    # NOTE: original used bare `rename(break_var = brk)`; `all_of()` makes
    # explicit that `brk`'s *value* (a column-name string), not a literal
    # column called `brk`, is being renamed -- avoids a tidyselect
    # deprecation warning.
    dplyr::rename(break_var = dplyr::all_of(brk)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(nums), ~ labelled::set_value_labels(labelled::user_na_to_na(.x), NULL)))

  break_data <- data_int |>
    dplyr::select(dplyr::all_of(nums), break_var) |>
    dplyr::group_by(break_var) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), stat_fns, .names = "{.col}_{.fn}")) |>
    tidyr::pivot_longer(cols = -break_var) |>
    tidyr::separate(name, into = c("variable", "stat"), sep = "_split__") |>
    tidyr::pivot_wider(names_from = break_var, values_from = value)

  total_data <- data_int |>
    dplyr::select(dplyr::all_of(nums)) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), stat_fns, .names = "{.col}_{.fn}")) |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    tidyr::separate(name, into = c("variable", "stat"), sep = "_split__") |>
    dplyr::rename(Total = value)

  result <- dplyr::full_join(break_data, total_data, by = dplyr::join_by(variable, stat))

  if (all(stats != "all")) result <- result |> dplyr::filter(stat %in% stats)
  if (order == "var") result <- result |> dplyr::arrange(variable, stat)

  return(result)
}


#' Numeric summary statistics for one or more variables
#'
#' @param data A data frame.
#' @param vars Character vector of numeric variable names. If `NULL`
#'   (default), uses every variable in `data`.
#' @param wide Logical. If `TRUE` (default), one column per variable. If
#'   `FALSE`, one column per statistic.
#' @param user_na Logical. If `TRUE` (default) and `wide = TRUE`, append a
#'   row tabulating any `labelled` user-defined missing values found in
#'   `vars`, which are otherwise excluded from the numeric statistics.
#' @param label Logical. If `TRUE`, use variable labels instead of names.
#'   Default `FALSE`.
#' @return A tibble, shape depending on `wide`.
#' @export
num_sum <- function(data,
                     vars = NULL,
                     wide = TRUE,
                     user_na = TRUE,
                     label = FALSE) {

  if (is.null(vars)) vars <- names(data)

  tbl <- data |>
    dplyr::select(dplyr::all_of(vars)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ labelled::set_value_labels(labelled::user_na_to_na(.x), NULL))) |>
    dplyr::summarise(dplyr::across(
      dplyr::everything(),
      list(
        split__mean = ~ mean(.x, na.rm = TRUE),
        split__median = ~ stats::median(.x, na.rm = TRUE),
        split__sd = ~ stats::sd(.x, na.rm = TRUE),
        split__min = ~ min(.x, na.rm = TRUE),
        split__max = ~ max(.x, na.rm = TRUE),
        split__n_missing = ~ sum(is.na(.x)),
        split__n_valid = ~ sum(!is.na(.x))
      ),
      .names = "{.col}_{.fn}"
    )) |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    tidyr::separate(name, into = c("variable", "stat"), sep = "_split__")

  if (isTRUE(label)) {
    # NOTE: original referenced `scls` and `df` here instead of `vars` and
    # `data` -- i.e. it silently relied on leftover globals from the
    # notebook's own trial code rather than its function arguments. Fixed
    # below to use `vars`/`data`.
    labs <- purrr::map_chr(vars, ~ data |>
        dplyr::select(dplyr::all_of(.x)) |>
        labelled::var_label(unlist = TRUE, null_action = "fill")
      ) |>
      tibble::as_tibble() |>
      dplyr::rename(label = value) |>
      dplyr::mutate(variable = vars)

    tbl <- tbl |>
      dplyr::full_join(labs, by = "variable") |>
      dplyr::mutate(variable = paste0(variable, ":- ", label)) |>
      dplyr::select(-label)
  }

  if (isTRUE(wide)) {
    tbl <- tbl |> tidyr::pivot_wider(names_from = variable, values_from = value)

    if (isTRUE(user_na)) {

      has_user_na <- purrr::map_int(vars, ~ length(labelled::na_values(data[[.x]]))) |> min() > 0

      if (has_user_na) {

        na_tbl <- purrr::map(vars, ~ data |>
            dplyr::filter(.data[[.x]] %in% labelled::na_values(.data[[.x]])) |>
            dplyr::mutate(dplyr::across(dplyr::all_of(.x), ~ labelled::to_factor(.x, levels = "prefixed"))) |>
            dplyr::pull(dplyr::all_of(.x)) |>
            forcats::fct_count() |>
            dplyr::filter(n != 0) |>
            tidyr::unite(!!rlang::sym(.x), c(f, n), sep = "; N = ") |>
            dplyr::mutate(id = dplyr::row_number())
          ) |>
          purrr::reduce(dplyr::full_join, by = "id") |>
          dplyr::select(-id)

        if (isTRUE(label)) {
          label_vect <- purrr::map(vars, function(xxx) {
            vect <- data |> dplyr::select(dplyr::all_of(xxx)) |> labelled::var_label(unlist = TRUE, null_action = "fill")
            vect_names <- names(vect)
            names(vect_names) <- paste0(vect_names, ":- ", vect)
            return(vect_names)
          }) |> unlist()

          na_tbl <- na_tbl |> dplyr::rename(dplyr::all_of(label_vect))
        }

        # NOTE: original used the deprecated `mutate_all()`; modernised to
        # `mutate(across(everything(), ...))`.
        tbl <- dplyr::bind_rows(tbl |> dplyr::mutate(dplyr::across(dplyr::everything(), labelled::to_factor)), na_tbl)
      }
    }
  } else if (isFALSE(wide)) {
    tbl <- tbl |> tidyr::pivot_wider(names_from = stat, values_from = value)
  }

  return(tbl)
}
