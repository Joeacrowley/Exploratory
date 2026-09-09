# deck

Helpers that wrap [officer](https://davidgohel.github.io/officer/) to build
PowerPoint decks on the bundled **Sage Mint Theme** template. This is a
packaged version of the helpers that `export_to_powerpoint.qmd` used to
define inline and dump to a `Scripts/ppt_functions/` folder.

## Install

```r
# from the Exploratory repo root
devtools::install("deck")
```

## Use

```r
library(deck)

ppt <- mint_pptx() |>
  add_title_slide("Quarterly review", subtitle = "Q3 2026") |>
  add_divider_slide("Results", header = "Section 1") |>
  full_width_content_slide("Headlines", c("Revenue up 4%", "Churn flat")) |>
  double_text_column_slide("Trade-offs", left_text = "Pros", right_text = "Cons") |>
  full_width_table_slide("Detail", "First rows", make_flex(head(mtcars, 15))) |>
  add_end_slide("Thanks", subtitle = "questions?")

print(ppt, target = "deck.pptx")
```

`mint_layouts(ppt)` lists the layouts the builders target and their real
placeholder labels — useful when a `ph_with()` call fails.

## Functions

| Function | Layout |
|---|---|
| `mint_pptx()` | opens the bundled template |
| `add_title_slide()` | Title: Green |
| `add_divider_slide()` | Divider: Green |
| `add_end_slide()` | End message: Green |
| `full_width_content_slide()` | Title and full width content |
| `double_text_column_slide()` | Title and two column content |
| `full_width_table_slide()` | Title and full width table with description |
| `full_width_chart_slide()` | Title and full width chart with description |
| `text_and_chart_slide()` | Text and chart with source |
| `make_flex()` | data frame -> styled flextable |
| `fit_to_height()` | shrink a flextable toward a height cap |

## Notes / known limits

- Every builder returns the deck invisibly, so it works in a pipe or as a
  bare statement (officer mutates the `rpptx` object in place).
- The package is tied to the one bundled template. A template change means
  editing `R/aaa-template-spec.R` (master name, layout names, placeholder
  labels, table-area geometry).
- `fit_to_height()` is a heuristic (trim padding, then uniform row height).
  Row heights are advisory in PowerPoint and font size is untouched, so it
  reduces overflow rather than guaranteeing a hard cap. A table that still
  doesn't fit afterwards raises a warning - shrink it editorially (fewer
  rows/columns, smaller font, split across slides).
- `add_divider_slide(footer = )` targets a real footer placeholder that sits
  low and short in this template; long footers overflow the slide.
- Chart helpers need `mschart` (Suggests).
