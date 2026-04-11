# Packages
library(tidyverse)
library(mschart)
library(officer)

# Set up powerpoint functions... 
functions_folder <- "I:/Workdocs/Analysis team/Code Standardisation/Code library/Joe/Powerpoint/functions"
map(list.files(functions_folder), ~source(paste0(functions_folder, "/", .x)))

# Create empty powerpoint
ppt <- create_empty_ppt()
