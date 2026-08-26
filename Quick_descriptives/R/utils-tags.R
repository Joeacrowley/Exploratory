# Internal metadata-tagging helpers ------------------------------------------
#
# The original 'quackery' versions of ctabs(), df_miss_brk() and
# df_miss_brk_h() called add_tags()/has_tag() from a small personal package
# called 'tagr'. Since that package's source/CRAN status wasn't confirmed,
# these two internal helpers replace it with an equivalent built on a plain
# "tags" attribute, so the package has no unverified external dependency.
# Swap these out for the real tagr calls if you'd rather keep using it.
#
# NOTE: in both places tagr was originally called (ctabs(), df_miss_brk()),
# the result of add_tags() was never reassigned back to the object being
# returned (e.g. `add_tags(result, p_lab, p_lab_n)` instead of
# `result <- add_tags(result, p_lab, p_lab_n)`), so the tag silently never
# attached and has_tag()/attributes()$tags always came back empty downstream.
# That reassignment has been fixed at both call sites in this port.

#' Attach lightweight metadata tags to an object
#'
#' Stores `...` as a list in the object's `"tags"` attribute. Used internally
#' to pass small bits of context (e.g. which statistic or breakdown variable
#' produced a table) from a summary function through to its formatting
#' counterpart, without changing the object's class or printed structure.
#'
#' @param x An object (typically a tibble) to tag.
#' @param ... Values to store, accessed positionally (`[[1]]`, `[[2]]`, ...)
#'   by the consuming function.
#' @return `x`, with a `"tags"` attribute attached.
#' @keywords internal
.add_tags <- function(x, ...) {
  attr(x, "tags") <- list(...)
  x
}

#' Check whether an object carries tags set by `.add_tags()`
#'
#' @param x An object to check.
#' @return `TRUE` if `x` has a non-NULL `"tags"` attribute, else `FALSE`.
#' @keywords internal
.has_tag <- function(x) {
  !is.null(attr(x, "tags"))
}
