library(tidyverse)
library(googlesheets)

id_path  <- "data/slippi_data"
id_files <- list.files(id_path, full.names = TRUE)

base_url <- "https://api.smash.gg/slippi/getBySet/"