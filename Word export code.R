library(flextable)
library(officer)
library(tidyverse)

setwd("I:/Workdocs/Analysis team/Code Standardisation/Word")
getwd()

template <- "I:/Workdocs/Analysis team/Code Standardisation/Word/Template - empty.docx"
x <- read_docx(template)

styles_info(x) %>% 
  select(style_type,style_id,style_name)

# Note: only those styles whose style_type is paragraph can be used to format text.
styles_info(x) %>% 
  select(style_type,style_id,style_name) %>% 
  filter(style_type == "paragraph")

# Demonstrate template with officer
read_docx(template) %>% 
  body_add_par(value = "Document title", style = "Document title") %>%
  body_add_par(value = "Normal text", style = "Normal") %>%
  body_add_par(value = "Normal text bold", style = "Normal: Bold") %>%
  body_add_par(value = "Normal text italic", style = "Normal: Italics") %>%
  body_add_par(value = "Heading 1: Numbered", style = "Heading 1: Numbered_") %>%
  body_add_par(value = "Numbered list level 1", style = "Numbered list: L1_") %>%
  body_add_par(value = "Numbered list level 2", style = "Numbered list: L2_") %>%
  body_add_par(value = "Numbered list level 3", style = "Numbered list: L3_") %>%
  body_add_par(value = "Heading 2: Numbered", style = "Heading 2: Numbered_") %>%
  body_add_par(value = "Bullet level 1", style = "Table: Bullet L1_") %>%
  body_add_par(value = "Bullet level 2", style = "Table: Bullet L2_") %>%
  body_add_par(value = "Bullet level 3", style = "Table: Bullet L3_") %>%
  body_add_par(value = "Heading 3: Numbered", style = "Heading 3: Numbered_") %>%
  body_add_par(value = "Heading 2 - unnumbered", style = "heading 2") %>%
  body_add_par(value = "Ruled heading style", style = "Ruled headings_") %>%
  print(target = "Template - export from R demonstrated.docx")
