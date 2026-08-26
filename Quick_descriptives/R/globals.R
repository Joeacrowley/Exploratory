# Declare tidy-eval column names used bare inside dplyr/tidyr verbs (e.g.
# `mutate(p = round(p * 100, 2))`) so `R CMD check`'s static analysis
# doesn't flag them as "no visible binding for global variable" NOTEs --
# these are column names resolved via data-masking at run time, not actual
# globals. This is the standard tidyverse-package workaround (see
# https://CRAN.R-project.org/package=dplyr, "programming with dplyr").
#
# This list was compiled while porting from 'quackery'; running
# `devtools::check()` after `devtools::document()` may surface a few more
# to add here.
#
#' @importFrom utils globalVariables
#' @importFrom rlang .data :=
NULL

utils::globalVariables(c(
  "p", "n", "f", "var", "total", "levels",
  "n_miss", "n_val", "na_miss", "na_val", "user_miss", "user_val",
  "name", "any_val", "num_val", "label",
  "break_var", "variable", "stat", "value", "id",
  "labs", "out2", "result", "sort"
))
