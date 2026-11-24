test_that("nc_palette() is a 5x5 hex matrix", {
  p <- nc_palette()
  expect_equal(dim(p), c(5L, 5L))
  expect_setequal(colnames(p), c("green", "purple", "pink", "blue", "orange"))
  expect_true(all(grepl("^#[0-9a-f]{6}$", p, ignore.case = TRUE)))
})

test_that(".chart_colours() resolves each colour spec to a named vector", {
  lv <- c("a", "b", "c")
  expect_named(.chart_colours(lv, "green"), lv)
  expect_length(.chart_colours(lv, "categorical"), 3L)
  expect_equal(unname(.chart_colours(lv, c("#111111", "#222222", "#333333"))),
               c("#111111", "#222222", "#333333"))
  expect_error(.chart_colours(lv, "teal"), "hue name")
  expect_warning(.chart_colours(letters[1:8], "categorical"), "recycling")
})

test_that("bar_chart() returns an ms_barchart for flat and palette fills", {
  d <- data.frame(grp = c("A", "B", "C"), n = c(9, 4, 7))
  expect_s3_class(bar_chart(d, "grp", "n"), "ms_barchart")
  expect_s3_class(bar_chart(d, "grp", "n", colour = "categorical",
                            horizontal = TRUE), "ms_barchart")
})

test_that("clustered / stacked / line builders return the right classes", {
  dg <- data.frame(grp = rep(c("A", "B"), each = 3),
                   sub = rep(c("x", "y", "z"), 2),
                   n   = c(4, 6, 2, 5, 3, 7))
  dl <- data.frame(month = rep(seq(as.Date("2024-01-01"), by = "month",
                                   length.out = 4), 2),
                   value = c(3, 5, 4, 6, 2, 3, 5, 4),
                   line  = rep(c("x", "y"), each = 4))
  expect_s3_class(clustered_bar_chart(dg, "grp", "n", "sub"), "ms_barchart")
  expect_s3_class(stacked_bar_chart(dg, "grp", "n", "sub", percent = TRUE),
                  "ms_barchart")
  expect_s3_class(line_chart(dl, "month", "value", "line"), "ms_linechart")
  expect_s3_class(line_chart(dl, "month", "value", "line", date = FALSE),
                  "ms_linechart")
})

test_that("styled charts drop onto slides and print to a file", {
  d  <- data.frame(grp = c("A", "B", "C"), n = c(9, 4, 7))
  dg <- data.frame(grp = rep(c("A", "B"), each = 3),
                   sub = rep(c("x", "y", "z"), 2), n = c(4, 6, 2, 5, 3, 7))
  ppt <- mint_pptx()
  n <- length(ppt)
  full_width_chart_slide(ppt, "Bar", chart = bar_chart(d, "grp", "n"))
  full_width_chart_slide(ppt, "Clustered",
                         chart = clustered_bar_chart(dg, "grp", "n", "sub"))
  expect_length(ppt, n + 2L)

  out <- tempfile(fileext = ".pptx")
  print(ppt, target = out)
  expect_true(file.exists(out))
})
