library(targets)
library(tarchetypes)

tar_option_set(packages = c(
  "dplyr","tidyr","lubridate","tsibble","purrr",
  "readabs","readrba","readr","stringr","cli","tibble"
))

source("R/utils_registry.R")
source("R/utils_fetch_abs.R")
#source("R/utils_fetch_rba.R")
#source("R/utils_clean_transform.R")

list(
  # Phase 0: Registry
  tar_target(registry_file, "data/schema.csv", format = "file"),
  tar_target(series_registry, read_registry(registry_file)),
  
  # Phase 1: LOAD
  tar_target(p1_abs_raw, fetch_abs_by_registry(series_registry)),
  # Phase 1.2: Load local ABS series
  tar_target(p1_abs_bad_ids, find_bad_ids(series_registry, p1_abs_raw))
)