library(dplyr)
library(tidyr)
library(lubridate)
library(readabs)

# Fetch a vector of ABS series IDs in one go; returns tidy (series_id, date, value, metadata...)
fetch_abs_series <- function(series_ids) {
  # read_abs() supports multiple series and returns a tidy tibble
  x <- readabs::read_abs(series = series_ids, show_progress_bars = FALSE)
  # Standardise columns
  x |>
    transmute(
      series_id = series,
      date      = as.Date(date),
      value     = value,
      unit      = series_unit,
      sa_flag   = seasonal_adjustment,
      table_no  = table_no,
      catalogue = catalogue
    ) |>
    arrange(series_id, date)
}

fetch_abs_by_registry <- function(reg) {
  ids <- reg |>
    filter(source == "ABS", !is.na(series_id), nzchar(series_id)) |>
    pull(series_id) |> unique()
  if (length(ids) == 0) return(tibble())
  fetch_abs_series(ids)
}
