library(targets)
library(tarchetypes)

tar_option_set(packages = c(
  "dplyr","tidyr","lubridate","tsibble","purrr",
  "readabs","readrba","readr","stringr","cli","tibble"
))

source("R/utils_registry.R")
source("R/utils_fetch_abs.R")
source("R/utils_fetch_rba.R")
#source("R/utils_clean_transform.R")

list(
  # Phase 0: Registry
  tar_target(registry_file, "data/schema.csv", format = "file"),
  tar_target(series_registry, read_registry(registry_file)),
  
  # Phase 1: LOAD
  # Phase 1.1: Load  ABS series
  tar_target(p11_abs_raw, fetch_abs_by_registry(series_registry)),
  #tar_target(p11_abs_bad, find_bad_abs_ids(series_registry, p11_abs_raw)),
  # Phase 1.2: Load local RBA series
  tar_target(p12_rba_raw, fetch_rba_by_registry(series_registry)),
  # tar_target(p12_rba_bad, find_bad_rba_ids(series_registry, p12_rba_raw))
  # Combine both ABS and RBA data object
  tar_target(
    p1_abs_rba_raw, 
    bind_rows(p11_abs_raw, p12_rba_raw) |> 
               arrange(date, series_id)
    )
)