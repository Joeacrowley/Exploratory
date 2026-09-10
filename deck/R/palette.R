# NatCen house colour palette: 5 hues, 5 shades each (darkest first).
.nc_colours <- list(
  green  = c("#00ab85", "#33bd9e", "#66ccb5", "#99decf", "#ccede8"),
  purple = c("#b053a1", "#bf75b5", "#d199c7", "#debad9", "#f0deed"),
  pink   = c("#f25c91", "#f57da8", "#f79ebf", "#fabfd4", "#fcdee8"),
  blue   = c("#7082d4", "#8f99de", "#abb5e5", "#c7cced", "#e3e5f5"),
  orange = c("#ff8200", "#ff9c33", "#ffb566", "#ffcc99", "#ffe5cc")
)
.nc_shades <- c("dark", "mid_dark", "mid", "light", "lightest")

#' NatCen colour palette
#'
#' The house palette as a character matrix: 5 hues (columns) by 5 shades
#' (rows, darkest first). Handy for picking colours by name, e.g.
#' `nc_palette()["mid", "green"]` or `nc_palette()[, "blue"]`.
#'
#' @return A 5x5 character matrix of hex colours with row/column names.
#' @export
#' @examples
#' nc_palette()
#' nc_palette()["mid_dark", ]
nc_palette <- function() {
  m <- do.call(cbind, .nc_colours)
  rownames(m) <- .nc_shades
  m
}

# Levels of the variable a chart colour is mapped to.
.levels_of <- function(x) {
  if (is.factor(x)) levels(x) else unique(as.character(x))
}

# Resolve the `colour` argument of the chart builders to a named vector of
# hex colours, one per level of the mapped variable.
#
# `colour` may be:
#   * a single colour (hex or R colour name) - handled by the builders as a
#     flat fill, not passed here
#   * a hue name ("green", "purple", "pink", "blue", "orange") - successive
#     shades of that hue, light end first dropped as needed
#   * "categorical"   - the mid-dark shade of each of the 5 hues
#   * "categorical10" - mid-dark then light shade of each hue (10 colours)
#   * a character vector of colours - used as given, recycled if short
.chart_colours <- function(levels, colour = "green") {
  n <- length(levels)

  cols <-
    if (length(colour) > 1) {
      unname(colour)
    } else if (colour %in% names(.nc_colours)) {
      .nc_colours[[colour]]
    } else if (colour == "categorical") {
      vapply(.nc_colours, `[[`, character(1), 2L)
    } else if (colour == "categorical10") {
      c(vapply(.nc_colours, `[[`, character(1), 2L),
        vapply(.nc_colours, `[[`, character(1), 4L))
    } else {
      stop("`colour` must be a hue name, \"categorical\", \"categorical10\", ",
           "or a vector of colours.", call. = FALSE)
    }

  if (n > length(cols)) {
    warning(sprintf("Palette has %d colours for %d levels; recycling.",
                    length(cols), n), call. = FALSE)
    cols <- rep(cols, length.out = n)
  }
  stats::setNames(cols[seq_len(n)], levels)
}

# Is `colour` a single flat fill rather than a palette spec?
.is_flat_colour <- function(colour) {
  length(colour) == 1 &&
    !colour %in% c(names(.nc_colours), "categorical", "categorical10")
}
