# Numeric-to-categorical binning helper -----------------------------------------
# Ported from the deprecated 'quackery' project (natural_breaks + user_breaks.Rmd).

#' Cut several numeric variables into a shared set of breaks
#'
#' Uses [cut()] to find a natural set of break points for `vars`, based on
#' the first variable, then widens the outer breaks so no value in any of
#' `vars` falls outside the range -- or applies a user-supplied set of
#' breaks (`brk`) directly.
#'
#' @param data A data frame.
#' @param vars Character vector of numeric variable names to bin. All share
#'   the same break points, so this works best when the variables have
#'   similar ranges/scales.
#' @param brk Optional numeric vector of user-defined break points. If
#'   supplied, `nbrk` is ignored and these breaks are used for every
#'   variable in `vars`.
#' @param nbrk Number of breaks to generate automatically. Default `10`.
#' @param stat Which statistic(s) to return: `"p"` (percentage, default),
#'   `"n"` (count), or `"b"` (both).
#' @param p_na Logical. If `FALSE` (default), percentages are recalculated
#'   to exclude missing values. If `TRUE`, missing values are included in
#'   the percentage base.
#' @param lab_style Integer `1` (default), `2`, or `3`, controlling how bin
#'   labels are formatted (e.g. `"Greater than 0 and up to 10"` vs.
#'   `"0 > & <= 10"` vs. `"0-10"`).
#' @return A tibble with one row per bin and one column per variable
#'   (or per variable/statistic combination if `stat = "b"`).
#' @export
natural_breaks <- function(data,
                            vars,
                            brk = NULL,
                            nbrk = 10,
                            stat = "p",
                            p_na = FALSE,
                            lab_style = 1) {

  if (is.null(brk)) {

    tbl <- purrr::map(vars, ~ data |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(.x), ~ labelled::set_value_labels(labelled::user_na_to_na(.x), NULL)),
        dplyr::across(dplyr::all_of(.x), ~ cut(.x, nbrk, include.lowest = TRUE, right = TRUE))
      ) |>
      dplyr::pull(dplyr::all_of(.x)) |>
      forcats::fct_count()
    )

    breaks_finder <- function(factor_levels_from_fct_count) {
      factor_levels_from_fct_count |>
        unlist() |>
        as.character() |>
        stringr::str_remove_all("\\(|\\]|\\[") |>
        stringr::str_split(",") |>
        unlist() |>
        as.numeric() |>
        unique() |>
        sort()
    }

    cuts_from_all <- purrr::map(tbl, ~ breaks_finder(.$f))

    cuts_for_all <- cuts_from_all[[1]]
    cuts_for_all[1] <- purrr::map_dbl(cuts_from_all, min) |> min()
    cuts_for_all[length(cuts_for_all)] <- purrr::map_dbl(cuts_from_all, max) |> max()
  }

  if (!is.null(brk)) cuts_for_all <- brk

  results <- purrr::map(vars, function(xxx) {

    int_tbl <- data |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(xxx), ~ labelled::set_value_labels(labelled::user_na_to_na(.x), NULL)),
        dplyr::across(dplyr::all_of(xxx), ~ cut(.x, cuts_for_all, include.lowest = TRUE, right = TRUE))
      ) |>
      dplyr::pull(dplyr::all_of(xxx)) |>
      forcats::fct_count(prop = TRUE)

    if (lab_style == 1) {
      int_tbl <- int_tbl |>
        dplyr::mutate(
          labs = as.character(f),
          labs = stringr::str_replace(labs, "\\(|\\[", "Greater than "),
          labs = stringr::str_replace(labs, ",", " and up to "),
          labs = stringr::str_remove(labs, "]")
        )
    } else if (lab_style == 2) {
      int_tbl <- int_tbl |>
        dplyr::mutate(
          labs = as.character(f),
          labs = stringr::str_replace(labs, "\\(|\\[", ""),
          labs = stringr::str_replace(labs, ",", " > & <= "),
          labs = stringr::str_remove(labs, "]")
        )
    } else if (lab_style == 3) {
      int_tbl <- int_tbl |>
        dplyr::mutate(
          labs = as.character(f),
          labs = stringr::str_replace(labs, "\\(|\\[", ""),
          labs = stringr::str_replace(labs, ",", "-"),
          labs = stringr::str_remove(labs, "]")
        )
    }

    int_tbl$labs <- factor(int_tbl$f, labels = stats::na.omit(int_tbl$labs))

    int_tbl <- int_tbl |> dplyr::select(labs, n, p)

    if (isFALSE(p_na)) {
      int_tbl <- int_tbl |>
        dplyr::group_by(is.na(labs)) |>
        dplyr::mutate(p = dplyr::case_when(is.na(labs) ~ NA, TRUE ~ n / sum(n, na.rm = TRUE)))
    }

    int_tbl |>
      dplyr::mutate(p = round(p * 100, 2)) |>
      tidyr::pivot_longer(cols = -labs) |>
      dplyr::mutate(var = xxx, .before = 1)
  }) |>
    dplyr::bind_rows() |>
    tidyr::pivot_wider(names_from = c(var, name), values_from = value)

  stat_keep <- "_p"
  if (stat == "n") stat_keep <- "_n"
  if (stat == "b") stat_keep <- c("_n", "_p")

  results <- results |> dplyr::select(labs, dplyr::ends_with(stat_keep))

  if (stat != "b") {
    results <- results |> dplyr::rename_with(~ stringr::str_remove(.x, "_n|_p"), .cols = -labs)
  }

  return(results)
}
