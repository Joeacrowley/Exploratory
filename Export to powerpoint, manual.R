# install.packages("mschart")   # if not already installed
# install.packages("officer")    # often used together with mscharts

library(tidyverse)
library(mschart)
library(officer)

fldr <- "I:/Workdocs/Analysis team/Code Standardisation/Code library/Joe/Powerpoint"
setwd(fldr)

# Example dataset
df <- data.frame(
  category = c("A", "B", "C", "D"),
  value = c(10, 23, 15, 8)
)
df

tbl <- df %>% flextable::flextable() %>% flextable::theme_zebra()
tbl

# Create a bar chart
chart <- ms_barchart(
  data = df,
  x = "category",
  y = "value"
)

# Preview in RStudio Viewer
print(chart)

# Create a PowerPoint and add the chart
ppt <- read_pptx() 

layout_summary(ppt)
layout_properties (ppt, layout = "Title and Content")
layout_properties (ppt, layout = "Two Content")

ppt <- ppt |>
  add_slide(layout = "Title and Content", master = "Office Theme") |>
  ph_with(chart, ph_location_type(type = "body")) |>
  ph_with("Example Bar Chart", ph_location_type(type = "title"))

ppt <- ppt |>
  add_slide(layout = "Two Content", master = "Office Theme") |>
  ph_with(chart, ph_location_label(ph_label = "Content Placeholder 2")) |>
  ph_with("Text of various kinds", ph_location_label(ph_label = "Content Placeholder 3")) |>
  ph_with("Example Bar Chart plus text",  ph_location_type(type = "title"))

ppt <- ppt |>
  add_slide(layout = "Two Content", master = "Office Theme") |>
  ph_with("Other text of various kinds", location = ph_location_left()) |>
  ph_with(c("Text of various kinds", "split across bullets"), location = ph_location_right()) |>
  ph_with("Text in two columns",  ph_location_type(type = "title"))

# Export the PowerPoint
print(ppt, target = "barchart_example.pptx")

ppt <- read_pptx(paste0(fldr,"/NatCen template layout.pptx")) 
layout_summary(ppt)
layout_properties(ppt) %>% select(name) %>% unique() %>% unlist %>% unname()
styles_to_use <- 
  layout_properties(ppt) %>% 
    filter(grepl("mint", name, ignore.case = T) | 
             name %in% c("Title and full width content",
                         "Title and two column content",
                         "Title and full width table with description", 
                         "Title and full width chart with description",
                         "Text and chart with source"
                         )) %>%
    select(name, type, type_idx, id, ph_label)

# ------------------------------------------------------------------------------
ppt <- read_pptx(paste0(fldr,"/NatCen empty slides.pptx")) 
styles_to_use %>% select(name) %>% unlist %>% unname() %>% unique

# ------------------------------------------------------------------------------
# title
styles_to_use %>% filter(name == "Title: Mint Green")

ppt <- ppt |>
  add_slide(layout = "Title: Mint Green", master = "Office Theme") |>
  ph_with("Title text", ph_location_label(ph_label = "Text Placeholder 9")) |>
  ph_with("Subtitle text", ph_location_label(ph_label = "Subtitle 2"))

# ------------------------------------------------------------------------------
# divider slide
styles_to_use %>% filter(name == "Divider: Mint Green")

ppt <- ppt |>
  add_slide(layout = "Divider: Mint Green", master = "Office Theme") |>
  ph_with("Footer text", ph_location_label(ph_label = "Straight Connector 3")) |>
  ph_with("Divider slide title text", ph_location_label(ph_label = "Text Placeholder 9")) |>
  ph_with("Header text, could use to name section", ph_location_label(ph_label = "Subtitle 2")) # alleged subtitle, not really. 

# ------------------------------------------------------------------------------
# Title and full width content 
styles_to_use %>% filter(name == "Title and full width content")

ppt <- ppt |>
  add_slide(layout = "Title and full width content", master = "Office Theme") |>
  ph_with("Title of full width slide with content", ph_location_label(ph_label = "Title 1")) |>
  ph_with("Content for slide", ph_location_label(ph_label = "Content Placeholder 2")) |>
  ph_with("Footer text apparently possible too", ph_location_label(ph_label = "Footer Placeholder 4")) 

# ------------------------------------------------------------------------------
# Title and two column content 
styles_to_use %>% filter(name == "Title and two column content")

ppt <- ppt |>
  add_slide(layout = "Title and two column content", master = "Office Theme") |>
  ph_with("Title of two column slide", ph_location_label(ph_label = "Title 1")) |>
  ph_with("Content for left side of slide", ph_location_type(type = "body", type_idx = 1), ) |>
  ph_with(c("Content for right side of slide", "See?"), ph_location_type(type = "body", type_idx = 2)) |>
  ph_with("Footer text apparently possible too", ph_location_label(ph_label = "Footer Placeholder 4")) 

# ------------------------------------------------------------------------------
# Title and full width table with description
styles_to_use %>% filter(name == "Title and full width table with description")

# use for big tables only really. 
ft_mtcars <- mtcars %>% rownames_to_column() %>% slice(1:8) %>% flextable::flextable() %>% flextable::theme_zebra()

ppt <- ppt |>
  add_slide(layout = "Title and full width table with description", master = "Office Theme") |>
  ph_with("Title of full width table with description", ph_location_label(ph_label = "Title 1")) |>
  ph_with("Table description text.", ph_location_type(type = "body"), ) |>
  ph_with(ft_mtcars, ph_location_type(type = "tbl")) |>
  ph_with("Footer text apparently possible too", ph_location_label(ph_label = "Footer Placeholder 4")) 

# ------------------------------------------------------------------------------
# Title and full width chart with description
styles_to_use %>% filter(name == "Title and full width chart with description")

# Create a bar chart
chart <- ms_barchart(
  data = mtcars %>% rownames_to_column() %>% slice(1:8),
  x = "rowname",
  y = "mpg"
)

ppt <- ppt |>
  add_slide(layout = "Title and full width chart with description", master = "Office Theme") |>
  ph_with("Title and full width chart with description", ph_location_label(ph_label = "Title 1")) |>
  ph_with("Chart description text.", ph_location_type(type = "body", type_idx = 1), ) |>
  ph_with(chart, ph_location_type(type = "chart")) |>
  ph_with("Source text", ph_location_type(type = "body", type_idx = 2)) |>
  ph_with("Footer text apparently possible too", ph_location_label(ph_label = "Footer Placeholder 4")) 

# ------------------------------------------------------------------------------
# Text on one side and chart on the other
styles_to_use %>% filter(name == "Text and chart with source")

ppt <- ppt |>
  add_slide(layout = "Text and chart with source", master = "Office Theme") |>
  ph_with("Text and chart with source", ph_location_label(ph_label = "Title 1")) |>
  ph_with("Chart description text.", ph_location_type(type = "body", type_idx = 1), ) |>
  ph_with(chart, ph_location_type(type = "chart")) |>
  ph_with("Text about chart that is of small font.", ph_location_type(type = "body", type_idx = 2)) |>
  ph_with("Footer text apparently possible too", ph_location_label(ph_label = "Footer Placeholder 4")) 

print(ppt, target = "NatCen example PowerPoint from R.pptx")

# ------------------------------------------------------------------------------
# End message
styles_to_use %>% filter(name == "End message: Mint Green")

ppt <- ppt |>
  add_slide(layout = "End message: Mint Green", master = "Office Theme") |>
  ph_with("Header apparently possible", ph_location_label(ph_label = "Straight Connector 8")) |>
  ph_with("End message: Mint Green", ph_location_type(type = "body", type_idx = 3)) |>
  ph_with(c("Text here","small font size","why so small?"), ph_location_type(type = "body", type_idx = 4)) |>
  ph_with(c("Text slightly to the right here"), ph_location_type(type = "body", type_idx = 5)) |> 
  ph_with("Footer text", ph_location_type(type = "body", type_idx = 6)) 

# ------------------------------------------------------------------------------
# Save output here
print(ppt, target = "NatCen example PowerPoint from R.pptx")

