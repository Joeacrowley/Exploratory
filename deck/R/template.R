#' Open the Sage Mint PowerPoint template
#'
#' Reads the "Sage Mint Theme" template bundled with the package and returns
#' an `rpptx` object ([officer::read_pptx()]) ready for the `*_slide()` builders.
#'
#' @param path Optional path to a different `.pptx` to use as the base deck.
#'   Defaults to the bundled template.
#'
#' @return An `rpptx` object ([officer::read_pptx()]).
#' @export
#' @examples
#' ppt <- mint_pptx()
#' ppt <- add_title_slide(ppt, "Hello", subtitle = "world")
#' print(ppt, target = tempfile(fileext = ".pptx"))
mint_pptx <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file(
      "extdata", "sage-mint-template.pptx",
      package = "deck", mustWork = FALSE
    )
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop("Template not found: ", path, call. = FALSE)
  }
  officer::read_pptx(path)
}

#' List the usable layouts in a deck
#'
#' A filtered view of [officer::layout_properties()] restricted to the
#' layouts the `*_slide()` builders target. Handy when a `ph_with()` call
#' fails and you need to see the real placeholder labels.
#'
#' @param slides An `rpptx` object ([officer::read_pptx()]), e.g. from [mint_pptx()].
#' @param placeholders If `TRUE` (default), return one row per placeholder
#'   with `name`, `type`, `type_idx` and `ph_label`. If `FALSE`, return the
#'   unique layout names only.
#'
#' @return A data frame, or a character vector when `placeholders = FALSE`.
#' @export
#' @examples
#' mint_layouts(mint_pptx(), placeholders = FALSE)
mint_layouts <- function(slides, placeholders = TRUE) {
  lp <- officer::layout_properties(slides)
  lp <- lp[lp$name %in% .mint_layout & lp$master_name == .mint_master, , drop = FALSE]
  if (!placeholders) {
    return(unique(lp$name))
  }
  lp <- lp[, c("name", "type", "type_idx", "id", "ph_label")]
  rownames(lp) <- NULL
  lp
}
