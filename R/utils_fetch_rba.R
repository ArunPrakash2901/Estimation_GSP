library(readrba)
library(dplyr)
library(tidyr)
library(lubridate)

# Pull RBA by table or by series code (registry decides which columns are set)
fetch_rba_by_registry <- function(reg) {
  rba_items <- reg |> filter(source == "RBA")
  if (nrow(rba_items) == 0) return(tibble())
  
  out <- list()
  # Option 1: fetch whole table if rba_table given (handy for G1, F1, etc.)
  tbls <- rba_items |> filter(!is.na(rba_table) & nzchar(rba_table)) |> pull(rba_table) |> unique()
  for (t in tbls) {
    dt <- readrba::read_rba(table_no = t, tidy = TRUE, to = Sys.Date())
    out[[paste0("tbl_", t)]] <- dt |>
      transmute(
        alias = series,                 # RBA returns a series label
        date  = date,
        value = value,
        rba_table = t
      )
  }

}