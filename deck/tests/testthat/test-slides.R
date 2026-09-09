test_that("mint_pptx() opens the bundled template", {
  ppt <- mint_pptx()
  expect_s3_class(ppt, "rpptx")
  expect_true(.mint_master %in% officer::layout_properties(ppt)$master_name)
})

test_that("mint_layouts() reports the targeted layouts", {
  ppt <- mint_pptx()
  expect_setequal(mint_layouts(ppt, placeholders = FALSE), unname(.mint_layout))
  lp <- mint_layouts(ppt)
  expect_true(all(c("name", "type", "type_idx", "ph_label") %in% names(lp)))
})

test_that("guard rejects a foreign deck", {
  expect_error(add_title_slide(officer::read_pptx(), "x"), "mint_pptx")
})

test_that("each builder adds one slide and returns the deck invisibly", {
  ppt <- mint_pptx()
  n <- length(ppt)

  expect_invisible(add_title_slide(ppt, "T", subtitle = "s"))
  expect_length(ppt, n + 1L)

  add_divider_slide(ppt, "D", header = "h", footer = "f")
  expect_length(ppt, n + 2L)

  add_end_slide(ppt, "E", subtitle = "s")
  expect_length(ppt, n + 3L)

  full_width_content_slide(ppt, "T", c("a", "b"), footer = "f")
  expect_length(ppt, n + 4L)

  double_text_column_slide(ppt, "T", left_text = "l", right_text = "r", footer = "f")
  expect_length(ppt, n + 5L)

  full_width_table_slide(ppt, "T", "desc", make_flex(head(mtcars, 5)), footer = "f")
  expect_length(ppt, n + 6L)

  full_width_table_slide(ppt, "T", "desc", make_flex(head(mtcars, 5)),
                         control_height = FALSE)
  expect_length(ppt, n + 7L)

  out <- tempfile(fileext = ".pptx")
  print(ppt, target = out)
  expect_true(file.exists(out))
})

test_that("chart slides work when mschart is available", {
  skip_if_not_installed("mschart")
  ch <- mschart::ms_barchart(
    data = data.frame(x = letters[1:3], y = 1:3),
    x = "x", y = "y"
  )
  ppt <- mint_pptx()
  n <- length(ppt)

  full_width_chart_slide(ppt, "T", text = "d", chart = ch, source = "s", footer = "f")
  expect_length(ppt, n + 1L)

  text_and_chart_slide(ppt, "T", "txt", ch, source = "s", footer = "f")
  expect_length(ppt, n + 2L)
})

test_that("make_flex() returns a flextable, with and without a caption", {
  expect_s3_class(make_flex(head(mtcars, 3)), "flextable")
  expect_s3_class(make_flex(head(mtcars, 3), caption = "Tab"), "flextable")
  expect_s3_class(make_flex(head(mtcars, 3), caption = FALSE), "flextable")
})

test_that("fit_to_height() shrinks a tall table", {
  tall <- make_flex(mtcars)
  before <- flextable::flextable_dim(tall)$height
  after <- flextable::flextable_dim(fit_to_height(tall, max_height = 3))$height
  expect_lt(after, before)
})
