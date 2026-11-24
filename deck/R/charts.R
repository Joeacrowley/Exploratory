# NatCen-styled mschart builders.
#
# Each returns a native `mschart` object (an editable Office chart) ready to
# drop onto a slide with full_width_chart_slide() / text_and_chart_slide().
# They share:
#   * .natcen_chart_style() - the house look (ticks out, no vertical grid,
#     faint horizontal grid, transparent series outline, legend placement)
#   * .chart_colours()      - resolves the `colour` argument to a palette
# See nc_palette() for the colours themselves.

# The house style. `...` passes extra args straight to chart_theme(), which
# merges rather than replaces, so callers can add e.g. title_y_rot = 0.
.natcen_chart_style <- function(chart, legend = "n", ...) {
  chart <- mschart::chart_ax_x(chart, major_tick_mark = "out")
  chart <- mschart::chart_ax_y(chart, major_tick_mark = "out")
  chart <- mschart::chart_data_stroke(chart, "transparent")
  mschart::chart_theme(
    chart,
    grid_major_line_x = officer::fp_border(width = 0),
    grid_minor_line_x = officer::fp_border(width = 0),
    grid_minor_line_y = officer::fp_border(width = 0),
    grid_major_line_y = officer::fp_border(style = "solid", color = "grey85"),
    legend_position = legend,
    ...
  )
}

#' NatCen-styled bar chart
#'
#' @param data A data frame.
#' @param x,y Column names (strings): the category axis and the values.
#' @param colour Bar colour. A single colour (hex or R colour name, the
#'   default NatCen green) fills every bar the same. A hue name
#'   (`"green"`, `"purple"`, `"pink"`, `"blue"`, `"orange"`),
#'   `"categorical"`, or a vector of colours gives one colour per bar.
#'   See [nc_palette()].
#' @param title,xlab,ylab Optional chart and axis labels.
#' @param data_labels Show the value on each bar.
#' @param horizontal Draw the bars horizontally.
#'
#' @return An [mschart::ms_barchart()], ready for [full_width_chart_slide()]
#'   or [text_and_chart_slide()].
#' @export
#' @examples
#' d <- data.frame(grp = c("A", "B", "C"), n = c(9, 4, 7))
#' bar_chart(d, "grp", "n")
#' bar_chart(d, "grp", "n", colour = "categorical", horizontal = TRUE)
bar_chart <- function(data, x, y, colour = "#00ab85",
                      title = NULL, xlab = NULL, ylab = NULL,
                      data_labels = TRUE, horizontal = FALSE) {
  dir  <- if (isTRUE(horizontal)) "horizontal" else "vertical"
  labs <- if (isTRUE(data_labels)) y else NULL

  if (.is_flat_colour(colour)) {
    chart <- mschart::ms_barchart(data, x = x, y = y, labels = labs)
    chart <- mschart::chart_settings(chart, dir = dir)
    chart <- mschart::chart_data_fill(chart, colour)
  } else {
    lv <- .levels_of(data[[x]])
    data[["group_proxy"]] <- data[[x]]
    chart <- mschart::ms_barchart(data, x = x, y = y,
                                  group = "group_proxy", labels = labs)
    chart <- mschart::chart_settings(chart, vary_colors = TRUE,
                                     grouping = "standard", overlap = 90,
                                     gap_width = 150, dir = dir)
    chart <- mschart::chart_data_fill(chart, .chart_colours(lv, colour))
  }

  chart <- mschart::chart_data_labels(chart, position = "outEnd")
  chart <- mschart::chart_labels(chart, title = title, xlab = xlab, ylab = ylab)
  chart <- .natcen_chart_style(chart, legend = "n")
  if (isTRUE(horizontal)) chart <- mschart::chart_theme(chart, title_y_rot = 0)
  chart
}

#' NatCen-styled clustered bar chart
#'
#' @inheritParams bar_chart
#' @param group Column name (string) mapped to the clustered series / colour.
#' @param colour Palette for the `group` series: a hue name,
#'   `"categorical"`, `"categorical10"`, or a vector of colours.
#'
#' @return An [mschart::ms_barchart()].
#' @export
#' @examples
#' d <- data.frame(
#'   grp = rep(c("A", "B"), each = 3),
#'   sub = rep(c("x", "y", "z"), 2),
#'   n   = c(4, 6, 2, 5, 3, 7)
#' )
#' clustered_bar_chart(d, x = "grp", y = "n", group = "sub")
clustered_bar_chart <- function(data, x, y, group, colour = "green",
                                title = NULL, xlab = NULL, ylab = NULL,
                                data_labels = TRUE, horizontal = FALSE) {
  dir  <- if (isTRUE(horizontal)) "horizontal" else "vertical"
  labs <- if (isTRUE(data_labels)) y else NULL
  fill <- .chart_colours(.levels_of(data[[group]]), colour)

  chart <- mschart::ms_barchart(data, x = x, y = y, group = group, labels = labs)
  chart <- mschart::chart_settings(chart, grouping = "clustered",
                                   overlap = -100, gap_width = 400, dir = dir)
  chart <- mschart::chart_data_labels(chart, position = "outEnd")
  chart <- mschart::chart_labels(chart, title = title, xlab = xlab, ylab = ylab)
  chart <- mschart::chart_data_fill(chart, fill)
  .natcen_chart_style(chart, legend = "b")
}

#' NatCen-styled stacked bar chart
#'
#' @inheritParams clustered_bar_chart
#' @param percent Cap the value axis at 100 (for pre-computed percentages).
#' @param num_fmt Number format for the data labels and value axis
#'   (e.g. `"0"`, `"0.0"`, `"0%"`).
#'
#' @return An [mschart::ms_barchart()].
#' @export
#' @examples
#' d <- data.frame(
#'   grp = rep(c("A", "B"), each = 3),
#'   sub = rep(c("x", "y", "z"), 2),
#'   n   = c(40, 35, 25, 55, 30, 15)
#' )
#' stacked_bar_chart(d, x = "grp", y = "n", group = "sub", percent = TRUE)
stacked_bar_chart <- function(data, x, y, group, colour = "green",
                              title = NULL, xlab = NULL, ylab = NULL,
                              data_labels = TRUE, horizontal = FALSE,
                              percent = FALSE, num_fmt = "0") {
  dir  <- if (isTRUE(horizontal)) "horizontal" else "vertical"
  labs <- if (isTRUE(data_labels)) y else NULL
  ymax <- if (isTRUE(percent)) 100 else NULL
  fill <- .chart_colours(.levels_of(data[[group]]), colour)

  chart <- mschart::ms_barchart(data, x = x, y = y, group = group, labels = labs)
  chart <- mschart::as_bar_stack(chart, dir = dir)
  chart <- mschart::chart_data_labels(chart, position = "ctr", num_fmt = num_fmt)
  chart <- mschart::chart_ax_y(chart, limit_max = ymax, num_fmt = num_fmt)
  chart <- mschart::chart_labels(chart, title = title, xlab = xlab, ylab = ylab)
  chart <- mschart::chart_data_fill(chart, fill)
  chart <- .natcen_chart_style(chart, legend = "b")
  chart <- mschart::chart_data_stroke(chart, values = "grey60")
  mschart::chart_data_line_width(chart, values = 1.5)
}

#' NatCen-styled line chart
#'
#' @inheritParams clustered_bar_chart
#' @param group Column name (string) mapped to one line per level.
#' @param date Format the x axis as `mmm-yy` dates.
#'
#' @return An [mschart::ms_linechart()].
#' @export
#' @examples
#' d <- data.frame(
#'   month = rep(seq(as.Date("2024-01-01"), by = "month", length.out = 4), 2),
#'   value = c(3, 5, 4, 6, 2, 3, 5, 4),
#'   line  = rep(c("x", "y"), each = 4)
#' )
#' line_chart(d, x = "month", y = "value", group = "line")
line_chart <- function(data, x, y, group, colour = "categorical",
                       title = NULL, xlab = NULL, ylab = NULL, date = TRUE) {
  stroke <- .chart_colours(.levels_of(data[[group]]), colour)

  chart <- mschart::ms_linechart(data, x = x, y = y, group = group)
  chart <- mschart::chart_labels(chart, title = title, xlab = xlab, ylab = ylab)
  chart <- .natcen_chart_style(chart, legend = "b")
  chart <- mschart::chart_data_stroke(chart, values = stroke)
  if (isTRUE(date)) chart <- mschart::chart_ax_x(chart, num_fmt = "mmm-yy")
  chart
}
