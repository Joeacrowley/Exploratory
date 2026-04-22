# Packages
library(tidyverse)
library(mschart)
library(officer)
library(here)

# Load PowerPoint functions from Scripts/ppt_functions/
functions_folder <- here::here("Scripts", "ppt_functions")
map(list.files(functions_folder, full.names = TRUE), source)

# Create empty powerpoint (reads my_template.pptx in project root by default)
ppt <- create_empty_ppt()
